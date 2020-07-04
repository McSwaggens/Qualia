#pragma once

#include "int.h"
#include "list.h"
#include "array.h"
#include "string.h"
#include "token.h"
#include "parser.h"

struct Semantic
{
	Scope scope;
	Parse_Info info;
};

void SemanticParse(Parse_Info info);

