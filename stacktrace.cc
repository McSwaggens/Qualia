#include "stacktrace.h"

#if defined(DEBUG)

#include <signal.h>
#include <execinfo.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

static void WriteStr(const char* s) {
	u64 len = 0;
	while (s[len]) len++;
	write(STDERR_FILENO, s, len);
}

static void WriteInt(s32 value) {
	char buf[12];
	s32 i = 11;
	if (value == 0) {
		buf[i--] = '0';
	} else {
		bool neg = value < 0;
		if (neg) value = -value;
		while (value > 0) {
			buf[i--] = '0' + (value % 10);
			value /= 10;
		}
		if (neg) buf[i--] = '-';
	}
	write(STDERR_FILENO, buf + i + 1, 11 - i);
}

// Write "0x..." hex string into buf (must be at least 19 bytes). Returns buf.
static char* FormatHex(u64 value, char* buf) {
	buf[0] = '0';
	buf[1] = 'x';
	const char* hex = "0123456789abcdef";
	for (s32 i = 15; i >= 0; i--) {
		buf[2 + i] = hex[value & 0xF];
		value >>= 4;
	}
	buf[18] = '\0';
	return buf;
}

static void PrintStackTrace() {
	void* frames[64];
	int count = backtrace(frames, 64);

	// Skip PrintStackTrace + caller (signal handler or Assert)
	int skip = 2;
	int naddrs = count - skip;
	if (naddrs <= 0) return;

	WriteStr("Stack trace:\n");

	// Format addresses as hex strings (subtract 1 to get call site, not return address)
	// argv: addr2line -e /proc/self/exe -f -C -p addr1 addr2 ... NULL
	constexpr int max_addrs = 62;
	if (naddrs > max_addrs) naddrs = max_addrs;

	// Resolve exe path before fork (child's /proc/self/exe would point to addr2line)
	char exe_path[4096];
	ssize_t exe_len = readlink("/proc/self/exe", exe_path, sizeof(exe_path) - 1);
	if (exe_len <= 0) return;
	exe_path[exe_len] = '\0';

	char hex_bufs[max_addrs][19];
	const char* argv[max_addrs + 6]; // program + 4 flags + addrs + NULL
	int ai = 0;
	argv[ai++] = "addr2line";
	argv[ai++] = "-e";
	argv[ai++] = exe_path;
	argv[ai++] = "-f";
	argv[ai++] = "-C";
	argv[ai++] = "-p";
	for (int i = 0; i < naddrs; i++) {
		u64 addr = (u64)frames[i + skip] - 1;
		FormatHex(addr, hex_bufs[i]);
		argv[ai++] = hex_bufs[i];
	}
	argv[ai] = nullptr;

	int pipefd[2];
	if (pipe(pipefd) != 0) goto fallback;

	{
		pid_t pid = fork();
		if (pid < 0) {
			close(pipefd[0]);
			close(pipefd[1]);
			goto fallback;
		}

		if (pid == 0) {
			// Child
			close(pipefd[0]);
			dup2(pipefd[1], STDOUT_FILENO);
			close(pipefd[1]);
			execvp("addr2line", (char* const*)argv);
			_exit(127);
		}

		// Parent
		close(pipefd[1]);

		// Read all output into buffer
		char output[64 * 256];
		ssize_t total = 0;
		ssize_t n;
		while ((n = read(pipefd[0], output + total, sizeof(output) - total - 1)) > 0) {
			total += n;
			if (total >= (ssize_t)sizeof(output) - 1) break;
		}
		output[total] = '\0';

		// Parse lines into (location, function) pairs
		struct Frame { const char* loc; int loc_len; const char* func; int func_len; };
		Frame parsed[64];
		int nframes = 0;
		int max_loc_len = 0;

		char* p = output;
		while (*p && nframes < 64) {
			// Find end of line
			char* eol = p;
			while (*eol && *eol != '\n') eol++;

			// Look for " at " separator (addr2line -p format: "function at file:line")
			char* at = nullptr;
			for (char* s = p; s + 4 <= eol; s++) {
				if (s[0] == ' ' && s[1] == 'a' && s[2] == 't' && s[3] == ' ') {
					at = s;
				}
			}

			if (at) {
				// function = [p, at), location = [at+4, eol)
				parsed[nframes].func = p;
				parsed[nframes].func_len = (int)(at - p);
				parsed[nframes].loc = at + 4;
				parsed[nframes].loc_len = (int)(eol - (at + 4));
			} else {
				// Unknown frame like "?? ??:0" — split on last space
				char* last_space = nullptr;
				for (char* s = p; s < eol; s++) {
					if (*s == ' ') last_space = s;
				}
				if (last_space) {
					parsed[nframes].func = p;
					parsed[nframes].func_len = (int)(last_space - p);
					parsed[nframes].loc = last_space + 1;
					parsed[nframes].loc_len = (int)(eol - (last_space + 1));
				} else {
					parsed[nframes].func = p;
					parsed[nframes].func_len = (int)(eol - p);
					parsed[nframes].loc = "??:0";
					parsed[nframes].loc_len = 4;
				}
			}

			// Normalize "??:?" to "??:0"
			if (parsed[nframes].loc_len == 4
				&& parsed[nframes].loc[0] == '?'
				&& parsed[nframes].loc[1] == '?'
				&& parsed[nframes].loc[2] == ':'
				&& parsed[nframes].loc[3] == '?') {
				parsed[nframes].loc = "??:0";
			}

			if (parsed[nframes].loc_len > max_loc_len)
				max_loc_len = parsed[nframes].loc_len;

			nframes++;
			p = (*eol) ? eol + 1 : eol;
		}

		// Skip innermost unknown frame (signal handler / crash trampoline)
		int start = 0;
		if (nframes > 0
			&& parsed[0].loc_len == 4
			&& parsed[0].loc[0] == '?'
			&& parsed[0].loc[1] == '?') {
			start = 1;
		}

		// Recalculate max_loc_len after skipping
		max_loc_len = 0;
		for (int i = start; i < nframes; i++) {
			if (parsed[i].loc_len > max_loc_len)
				max_loc_len = parsed[i].loc_len;
		}

		// Print in reverse order (deepest frame first, crash site last), column-aligned
		for (int i = nframes - 1; i >= start; i--) {
			write(STDERR_FILENO, parsed[i].loc, parsed[i].loc_len);
			WriteStr(":");
			int padding = max_loc_len - parsed[i].loc_len;
			for (int s = 0; s < padding; s++) write(STDERR_FILENO, " ", 1);
			WriteStr(" ");
			write(STDERR_FILENO, parsed[i].func, parsed[i].func_len);
			WriteStr("\n");
		}

		close(pipefd[0]);
		int status;
		waitpid(pid, &status, 0);
	}
	return;

fallback:
	for (int i = naddrs - 1; i >= 0; i--) {
		WriteStr(hex_bufs[i]);
		WriteStr(": ??\n");
	}
}

static void CrashHandler(int sig) {
	const char* name = "Unknown signal";
	switch (sig) {
		case SIGSEGV: name = "SIGSEGV (Segmentation fault)"; break;
		case SIGBUS:  name = "SIGBUS (Bus error)";           break;
		case SIGFPE:  name = "SIGFPE (Floating point exception)"; break;
		case SIGILL:  name = "SIGILL (Illegal instruction)"; break;
		case SIGABRT: name = "SIGABRT (Abort)";              break;
	}

	WriteStr("Crash: ");
	WriteStr(name);
	WriteStr("\n");

	PrintStackTrace();

	_exit(1);
}

static void InitCrashHandler() {
	struct sigaction sa;
	__builtin_memset(&sa, 0, sizeof(sa));
	sa.sa_handler = CrashHandler;
	sa.sa_flags = SA_RESETHAND;

	sigaction(SIGSEGV, &sa, null);
	sigaction(SIGBUS,  &sa, null);
	sigaction(SIGFPE,  &sa, null);
	sigaction(SIGILL,  &sa, null);
	sigaction(SIGABRT, &sa, null);
}

#else

static void InitCrashHandler() {}
static void PrintStackTrace() {}

#endif
