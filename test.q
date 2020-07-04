
struct Person:
	team      : uint8
	health    : int16
	max_health: int16
	state     : State
	position  : Vector2

enum State:
	Idle     = 0
	Moving   = 1
	Mining   = 2
	Building = 3
	Fighting = 4

struct Resource:
	type: ResourceType
	amount: int32

enum ResourceType:
	Food  = 0
	Wood  = 1
	Stone = 2
	Gold  = 3

struct Vector2:
	x: float32
	y: float32


Main():
	v: Vector2

	person: Person
	person.max_health = 100
	person.health = person.max_health
	person.team = 0
	person.state = State.Idle
	person.position = v

	laser_damage := 10
	laser_enabled: bool = false

	IsDead(p: Person) -> bool:
		return p.health <= 0

	is_cool := alpha and beta and not delta^charlie("because everyone hates charlie.")

	while !person.IsDead():
		FireTheLaser(*person, laser_damage)

		FireTheLaser(p: *Person, damage: int):
			if laser_enabled:
				p.health -= damage
				Print("Bzzzt!\n")
			else:
				Print("The laser wasn't enabled sire... Try again?\n")
				laser_enabled = true

		if person.IsDead():
			Print("Oof, the person was burnt to a crisp!\n")
			return
		else:
			Print("Give em another zap!\n")

