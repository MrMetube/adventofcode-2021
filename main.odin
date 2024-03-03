package main

// import time "../computer_enhance/haversine/perftime"
import pq "core:container/priority_queue"
import "core:fmt"
import "core:mem"
import "core:slice"
import "core:strconv"
import "core:strings"
import "core:unicode"

main :: proc() {
	// think about memory leaks, but as this just runs once the OS will also just free all memory used 🤷‍♂️
	// tracking_allocator : mem.Tracking_Allocator
	// mem.tracking_allocator_init(&tracking_allocator, context.allocator)
	// context.allocator = mem.tracking_allocator(&tracking_allocator)

	// reset_tracking_allocator :: proc(a:^mem.Tracking_Allocator) {
	// 	for key, value in a.allocation_map {
	// 		fmt.printf("%v: Leaked %v bytes\n", value.location, value.size)
	// 	}
	// 	mem.tracking_allocator_clear(a)
	// }

	// defer reset_tracking_allocator(&tracking_allocator)

	days := []day {
		day{num = 01, run = day01},
		day{num = 02, run = day02},
		day{num = 03, run = day03},
		day{num = 04, run = day04},
		day{num = 05, run = day05},
		day{num = 06, run = day06},
		day{num = 07, run = day07},
		day{num = 08, run = day08},
		day{num = 09, run = day09},
		day{num = 10, run = day10},
		day{num = 11, run = day11},
		day{num = 12, run = day12},
		day{num = 13, run = day13},
		day{num = 14, run = day14},
		day{num = 15, run = day15},
		day{num = 16, run = day16},
	}
	for d in days {
		path := fmt.aprintf("./input/%02d.txt", d.num)
		lines, success := read_lines(path)
		if !success do fmt.panicf("Could not read file: %v", path)

		part1, part2 := d.run(lines)
		if part1 == 0 && part2 == 0 do continue
		fmt.printf("Day %2d  |  % 16d  |  % 16d\n", d.num, part1, part2)
	}
}

dayproc :: #type proc(lines: []string) -> (part1: int, part2: int)

day :: struct {
	num: int,
	run: dayproc,
}

day16 :: proc(lines: []string) -> (part1: int, part2: int) {

	TypeId :: enum {
		sum          = 0,
		product      = 1,
		minimum      = 2,
		maximum      = 3,
		literal      = 4,
		greater_than = 5,
		less_than    = 6,
		equal_to     = 7,
	}
	Expression :: struct {
		type:   TypeId,
		values: [dynamic]Packet,
	}
	Literal :: u64
	Packet :: union {
		Literal,
		Expression,
	}

	PacketParser :: struct {
		bits:        [dynamic]u8,
		count:       u32,
		version_sum: u32,
	}

	append_to_value :: #force_inline proc(p: ^PacketParser, size: u32, value: ^u64 = nil) {
		for i in 0 ..< size {
			value^ <<= 1
			value^ |= u64(pop_front(&p.bits))
		}
		p.count += size
	}

	parse_value :: #force_inline proc(p: ^PacketParser, size: u32) -> (value: u32) {
		for i in 0 ..< size {
			value <<= 1
			value |= u32(pop_front(&p.bits))
		}
		p.count += size
		return value
	}

	eat_bit :: #force_inline proc(p: ^PacketParser) -> (value: u8) {
		value = pop_front(&p.bits)
		p.count += 1
		return value
	}

	parse_packet :: proc(p: ^PacketParser) -> (result: Packet) {
		VERSION_LEN :: 3
		TYPE_LEN :: 3
		LENGTH_TYPE_ID_LEN :: 1
		LITERAL_VALUE_LEN :: 4

		TOTAL_LENGTH_IN_BITS_LEN :: 15
		NUMBER_OF_SUBPACKETS_IMMEDIATLY_CONTAINED :: 11

		version := parse_value(p, VERSION_LEN)
		p.version_sum += version
		type := cast(TypeId)parse_value(p, TYPE_LEN)

		switch type {
		case .literal:
			literal: u64
			for eat_bit(p) != 0 {
				append_to_value(p, LITERAL_VALUE_LEN, &literal)
			}
			append_to_value(p, LITERAL_VALUE_LEN, &literal)
			result = literal
		case .sum, .product, .minimum, .maximum, .greater_than, .less_than, .equal_to:
			length_type_id := parse_value(p, LENGTH_TYPE_ID_LEN)

			values: [dynamic]Packet
			if length_type_id == 0 {
				total_length_in_bits := parse_value(p, TOTAL_LENGTH_IN_BITS_LEN)
				target := p.count - 1 + total_length_in_bits // -1 cause we are already at the first bit and only need length-1 more

				for p.count < target {
					append(&values, parse_packet(p))
				}
			} else {
				number_of_subpackets := parse_value(p, NUMBER_OF_SUBPACKETS_IMMEDIATLY_CONTAINED)
				for sub in 0 ..< number_of_subpackets {
					a := parse_packet(p)
					append(&values, a)
				}
			}

			e := Expression {
				type   = type,
				values = values,
			}
			result = e
		}
		return
	}

	evaluate :: proc(packet: Packet) -> (result: u64) {
		switch p in packet {
		case Literal:
			result = p
		case Expression:
			switch p.type {
			case .sum:
				for value in p.values {
					result += evaluate(value)
				}
			case .product:
				result = 1
				for value in p.values {
					result *= evaluate(value)
				}
			case .minimum:
				result = 1 << 31
				for value in p.values {
					result = min(result, evaluate(value))
				}
			case .maximum:
				for value in p.values {
					result = max(result, evaluate(value))
				}
			case .literal:
				// NOTE: literals are already handled in parsing, see above
				assert(false, "unreachable")
				return 0
			case .greater_than:
				assert(len(p.values) == 2)
				result = evaluate(p.values[0]) > evaluate(p.values[1]) ? 1 : 0
			case .less_than:
				assert(len(p.values) == 2)
				result = evaluate(p.values[0]) < evaluate(p.values[1]) ? 1 : 0
			case .equal_to:
				assert(len(p.values) == 2)
				result = evaluate(p.values[0]) == evaluate(p.values[1]) ? 1 : 0
			}
		}
		return result
	}
	hex_stream := lines[0]

	append_digit :: proc(bits: ^[dynamic]u8, value: u8) {
		for i := 3; i >= 0; i -= 1 {
			mask: u8 = 1 << u8(i)
			append(bits, value & mask != 0 ? 1 : 0)
		}
	}

	bits: [dynamic]u8
	for r in hex_stream {
		switch r {
		case '0' ..= '9':
			append_digit(&bits, u8(r - '0'))
		case 'A' ..= 'F':
			append_digit(&bits, u8(r - 'A') + 10)
		case:
			assert(false, "unreachable")
		}
	}

	p := PacketParser {
		bits = bits,
	}

	result := parse_packet(&p)

	return int(p.version_sum), int(evaluate(result))
}
day15 :: proc(lines: []string) -> (part1: int, part2: int) {
	// the cost of the step
	g :: #force_inline proc "contextless" (field: [][]int, x, y: int, is_large := false) -> int {
		if !is_large do return field[x][y]

		xtimes := x / len(field)
		ytimes := y / len(field[0])

		xpos := x % len(field)
		ypos := y % len(field[0])

		value := field[xpos][ypos] + xtimes + ytimes
		for value > 9 do value -= 9

		return value
	}
	// the approximate cost of the path
	h :: #force_inline proc "contextless" (field: [][]int, x, y: int, is_large := false) -> int {
		current := Vector2{x, y}
		goal: Vector2
		if !is_large {
			goal = Vector2{len(field) - 1, len(field[0]) - 1}
		} else {
			goal = Vector2{len(field) * 5 - 1, len(field[0]) * 5 - 1}
		}
		distance := goal - current
		return abs(distance.x) + abs(distance.y)
	}
	// parse
	field := make([][]int, len(lines))
	for line, row in lines {
		field[row] = make([]int, len(line))
		for _, col in line {
			field[row][col] = strconv.atoi(line[col:col + 1])
		}
	}
	// find path
	state :: struct {
		pos:  Vector2,
		g, h: int,
	}

	a_star :: proc(lowest: ^map[Vector2]int, field: [][]int, is_large: bool) -> state {
		neighbours :: []Vector2{{0, 1}, {1, 0}, {0, -1}, {-1, 0}}

		order_by_cost :: proc(a, b: state) -> bool {return a.g + a.h < b.g + b.h}

		start := state {
			h = h(field, 0, 0, is_large),
		}

		goal := Vector2{len(field), len(field[0])}
		if is_large do goal *= 5
		goal -= 1

		todo: pq.Priority_Queue(state)
		pq.init(&todo, order_by_cost, pq.default_swap_proc(state), 1024)
		pq.push(&todo, start)

		for {
			s := pq.pop(&todo)

			if s.pos.x == goal.x && s.pos.y == goal.y {
				return s
			}

			for n in neighbours {
				delta := s.pos + n
				fieldsize_x := len(field) * (is_large ? 5 : 1)
				fieldsize_y := len(field) * (is_large ? 5 : 1)
				if delta.x >= 0 && delta.x < fieldsize_x && delta.y >= 0 && delta.y < fieldsize_y {
					next := state {
						pos = delta,
						g   = s.g + g(field, delta.x, delta.y, is_large),
						h   = h(field, delta.x, delta.y, is_large),
					}
					if next.pos in lowest {
						prev := lowest[next.pos]
						if prev <= next.g do continue
					}
					lowest[next.pos] = next.g
					pq.push(&todo, next)
				}
			}
		}
	}
	lowest, lowest_large: map[Vector2]int

	lowest_risk := a_star(&lowest, field, false).g
	lowest_risk_large := a_star(&lowest_large, field, true).g

	return lowest_risk, lowest_risk_large
}

day14 :: proc(lines: []string) -> (part1: int, part2: int) {
	fmt.print("TODO: ")
	node :: struct {
		next:  ^node,
		value: u8,
	}
	engine :: struct {
		rules:      map[[2]u8]u8,
		start, end: ^node,
	}

	// time.begin_profiling()
	// defer time.end_profiling()
	e := engine{}
	first := lines[0]
	for _, i in first {
		n := new(node)
		n.value = first[i]
		if e.end == nil {
			e.end, e.start = n, n
			n.next = nil
		} else {
			e.end.next = n
			n.next = nil
			e.end = n
		}
	}

	// parse
	for line, index in lines {
		if index < 2 do continue
		split := strings.split(line, " -> ")
		in_s := split[0]
		out_s := split[1]
		assert(len(in_s) == 2)
		assert(len(out_s) == 1)

		input := [2]u8{in_s[0], in_s[1]}
		output := out_s[0]
		e.rules[input] = output
	}

	counts_10, counts_40: map[u8]int
	least_common_10, most_common_10 := 1 << 31, 0
	// solve
	for step in 1 ..= 21 {
		using e
		a, b: ^node
		a = e.start
		b = a.next
		for b != nil {
			input := [2]u8{a.value, b.value}
			replacement, ok := rules[input]
			if ok {
				next, error := new(node)
				if error != nil do fmt.panicf("couldnt alloc next, %s", error)

				next.value = replacement
				n := next
				if a != e.end {
					n.next = a.next
					a.next = n
				} else {
					a.next = n
					n.next = nil
				}
			}
			a = b
			b = b.next
		}

		if step == 10 {
			for node := e.start; node != nil; node = node.next {
				counts_10[node.value] += 1
			}
			for _, count in counts_10 {
				least_common_10 = min(least_common_10, count)
				most_common_10 = max(most_common_10, count)
			}
		}
	}

	for node := e.start; node != nil; node = node.next {
		counts_40[node.value] += 1
	}

	least_common_40, most_common_40 := 1 << 31, 0
	for _, count in counts_40 {
		least_common_40 = min(least_common_40, count)
		most_common_40 = max(most_common_40, count)
	}

	return most_common_10 - least_common_10, most_common_40 - least_common_40
}

day13 :: proc(lines: []string) -> (part1: int, part2: int) {
	//parse
	paper_type :: struct {
		dots: map[Vector2]bool,
		size: Vector2,
	}
	paper: paper_type
	fold_line_index: int
	for line, index in lines {
		if len(line) == 0 {
			fold_line_index = index + 1
			break
		}
		split := strings.split(line, ",")
		c := Vector2{strconv.atoi(split[0]), strconv.atoi(split[1])}
		paper.size.x = max(paper.size.x, c.x)
		paper.size.y = max(paper.size.y, c.y)
		paper.dots[c] = true
	}
	folds: [dynamic]Vector2
	for i in fold_line_index ..< len(lines) {
		line := lines[i]
		rep, _ := strings.replace(line, "fold along ", "", 1)
		split := strings.split(rep, "=")
		fold: Vector2 = {-1, -1}
		switch split[0] {
		case "y":
			fold.y = strconv.atoi(split[1])
		case "x":
			fold.x = strconv.atoi(split[1])
		}
		append(&folds, fold)
	}

	// fold
	visible_after_first_fold: int
	for fold, fold_index in folds {
		Y := fold.x == -1 && fold.y != -1
		if Y do paper.size.y = fold.y
		else do paper.size.x = fold.x

		todo: [dynamic]^Vector2
		for dot in paper.dots {
			if Y && dot.y > paper.size.y {
				delete_key(&paper.dots, dot)
				distance := dot.y - fold.y
				new_dot := new(Vector2)
				new_dot^ = Vector2{dot.x, fold.y - distance}
				append(&todo, new_dot)
			} else if !Y && dot.x > paper.size.x {
				delete_key(&paper.dots, dot)
				distance := dot.x - fold.x
				new_dot := new(Vector2)
				new_dot^ = Vector2{fold.x - distance, dot.y}
				append(&todo, new_dot)
			}
		}

		for dot in todo do paper.dots[dot^] = true

		if fold_index == 0 {
			visible_after_first_fold = len(paper.dots)
		}
	}

	chart := make([][]bool, paper.size.x)
	for i in 0 ..< paper.size.x do chart[i] = make([]bool, paper.size.y)
	for dot in paper.dots {
		chart[dot.x][dot.y] = true
	}
	// fmt.println()
	// for col in 0 ..< paper.size.y {
	// 	for row in 0 ..< paper.size.x {
	// 		d := chart[row][col]
	// 		fmt.print(d ? '#' : ' ')
	// 	}
	// 	fmt.println()
	// }
	// fmt.println()

	return visible_after_first_fold, 0
}

day12 :: proc(lines: []string) -> (part1: int, part2: int) {
	cave :: struct {
		value:    int,
		is_small: bool,
	}
	start :: cave {
		value    = 0,
		is_small = true,
	}
	end :: cave {
		value    = 1,
		is_small = true,
	}
	caves: map[string]cave = {
		"start" = start,
		"end"   = end,
	}

	connection :: map[cave]bool
	connections: map[cave]connection

	// parse
	for line in lines {
		split := strings.split(line, "-")
		a := split[0]
		b := split[1]

		cave_a, ok_a := caves[a]
		if !ok_a {
			cave_a = cave {
				value    = len(caves),
				is_small = unicode.is_lower(rune(a[0])),
			}
			caves[a] = cave_a
		}
		cave_b, ok_b := caves[b]
		if !ok_b {
			cave_b = cave {
				value    = len(caves),
				is_small = unicode.is_lower(rune(b[0])),
			}
			caves[b] = cave_b
		}

		if cave_a not_in connections do connections[cave_a] = new(connection)^
		if cave_b not_in connections do connections[cave_b] = new(connection)^

		(&connections[cave_a])^[cave_b] = true
		(&connections[cave_b])^[cave_a] = true
	}

	path :: struct {
		small_visited:       []bool,
		current:             cave,
		visited_small_twice: bool,
	}

	visit :: #force_inline proc(using p: ^path, c: cave) {
		small_visited[c.value] = c.is_small
		current = c
	}

	start_path := path {
		small_visited = make([]bool, len(caves)),
	}
	visit(&start_path, start)
	todo: [dynamic]path = {start_path}

	// find the paths
	valid_paths, valid_paths_with_small: int
	for len(todo) > 0 {
		p := pop(&todo)
		con := connections[p.current]
		for other in con {
			if other.value == end.value {
				if !p.visited_small_twice do valid_paths += 1
				valid_paths_with_small += 1
				continue
			}

			small_twice: bool
			if other.is_small && p.small_visited[other.value] {
				if p.visited_small_twice || other.value == start.value {
					continue
				}
				small_twice = true
			}

			new := path {
				small_visited = make([]bool, len(caves)),
			}
			for _, index in p.small_visited do new.small_visited[index] = p.small_visited[index]
			new.visited_small_twice = small_twice || p.visited_small_twice

			visit(&new, other)
			append(&todo, new)
		}
	}

	return valid_paths, valid_paths_with_small
}

day11 :: proc(lines: []string) -> (part1: int, part2: int) {
	octopus :: struct {
		current, new: u8,
		flashed:      bool,
	}
	octopuses := make([][]octopus, len(lines))

	parse: for line, row in lines {
		row_array := make([]octopus, len(line))
		for __, col in line {
			row_array[col].current = line[col] - '0'
		}
		octopuses[row] = row_array
	}

	flash_count: int
	all_flash_step: int
	simulate: for step in 1 ..= 1000 {

		// increment the next values
		for r, row in octopuses do for o, col in r {
			octopuses[row][col].new = o.current + 1
		}

		flash :: proc(
			octopuses: ^[][]octopus,
			flashed: ^[dynamic]Vector2,
			row, col: int,
		) -> (
			flash_count: int,
		) {
			o := octopuses[row][col]
			if !o.flashed && o.new > 9 {
				octopuses[row][col].flashed = true
				flash_count += 1
				for d_row in -1 ..= 1 {
					for d_col in -1 ..= 1 {
						if d_row == 0 && d_col == 0 do continue
						n_row, n_col := row + d_row, col + d_col
						if index_in_bounds(n_row, octopuses^) &&
						   index_in_bounds(n_col, octopuses[0]) {
							if !octopuses[n_row][n_col].flashed {
								octopuses[n_row][n_col].new += 1
								append(flashed, Vector2{n_row, n_col})
							}
						}
					}
				}
			}
			return
		}

		// flash 9s
		flashed: [dynamic]Vector2
		for r, row in octopuses do for o, col in r {
			count := flash(&octopuses, &flashed, row, col)
			if step <= 100 {
				flash_count += count
			}
		}

		for len(flashed) > 0 {
			coord := pop(&flashed)
			row, col := coord.x, coord.y
			o := octopuses[row][col]
			count := flash(&octopuses, &flashed, row, col)
			if step <= 100 {
				flash_count += count
			}
		}

		if all_flash_step == 0 {
			all_flash := true
			all_flash_loop: for r, row in octopuses do for o, col in r {
				if !o.flashed {
					all_flash = false
					break all_flash_loop
				}
			}
			if all_flash do all_flash_step = step
		}

		// reset flashed and update x
		for r, row in octopuses do for o, col in r {
			octopuses[row][col].current = o.new > 9 ? 0 : o.new
			octopuses[row][col].flashed = false
		}
	}
	return flash_count, all_flash_step
}

day10 :: proc(lines: []string) -> (part1: int, part2: int) {
	bracket :: enum {
		brace  = 3,
		square = 57,
		curly  = 1197,
		angle  = 25137,
	}
	pair :: struct {
		type:       bracket,
		is_opening: bool,
		is_closed:  bool,
	}
	using bracket

	error_score: int
	autocomplete_scores: [dynamic]int

	for_lines: for line in lines {
		pair_line: [dynamic]pair
		stack: [dynamic]^pair
		for r in line {
			switch r {
			case '(':
				append(&pair_line, pair{type = brace, is_opening = true})
			case '[':
				append(&pair_line, pair{type = square, is_opening = true})
			case '{':
				append(&pair_line, pair{type = curly, is_opening = true})
			case '<':
				append(&pair_line, pair{type = angle, is_opening = true})

			case ')':
				append(&pair_line, pair{type = brace})
			case ']':
				append(&pair_line, pair{type = square})
			case '}':
				append(&pair_line, pair{type = curly})
			case '>':
				append(&pair_line, pair{type = angle})
			}
		}
		current: ^pair
		found_error := false
		for _, index in pair_line {
			current = &pair_line[index]
			if current.is_opening do append(&stack, current)
			else {
				top := stack[len(stack) - 1]
				if current.type != top.type {
					if !found_error {
						error_score += int(top.type)
						found_error = true
					}
				} else {
					top.is_closed = true
					pop(&stack)
				}
			}
		}
		if found_error do continue for_lines

		score: int
		#reverse for p in pair_line {
			if p.is_opening && !p.is_closed {
				score *= 5
				switch p.type {
				case brace:
					score += 1
				case square:
					score += 2
				case curly:
					score += 3
				case angle:
					score += 4
				}
			}
		}

		append(&autocomplete_scores, score)
	}

	slice.sort(autocomplete_scores[:])

	return error_score, autocomplete_scores[len(autocomplete_scores) / 2]
}

day09 :: proc(lines: []string) -> (part1: int, part2: int) {
	neighbours :: [4]Vector2{Vector2{0, 1}, Vector2{0, -1}, Vector2{1, 0}, Vector2{-1, 0}}

	cell :: struct {
		value:              u8,
		low_point, visited: bool,
	}

	low_point_sum: int

	chart := make([][]cell, len(lines))
	// parse
	for line, row in lines {
		chart[row] = make([]cell, len(line))
		for r, col in line {
			chart[row][col].value = u8(r) - '0'
		}
	}


	// low points
	for line, x in chart {
		for cell, y in line {
			smallest := true
			for offset in neighbours {
				dx := x + offset.x
				dy := y + offset.y
				if index_in_bounds(dx, chart) && index_in_bounds(dy, line) {
					if chart[dx][dy].value <= cell.value {
						smallest = false
						break
					}
				}
			}
			if smallest {
				chart[x][y].low_point = true
				low_point_sum += 1 + int(cell.value)
			}
		}
	}

	// basins
	basins_sizes: [dynamic]int
	outer: for line, x in chart {
		for cell, y in line {
			if !cell.low_point do continue

			current_basin_size: int
			cells_to_check: [dynamic]Vector2

			append(&cells_to_check, Vector2{x, y})

			for len(cells_to_check) > 0 {
				current := pop(&cells_to_check)
				// Note: we have visited cells in here, cause a cell may be added 
				// then added again and visited and only then the first instance is worked on
				if chart[current.x][current.y].visited do continue
				current_basin_size += 1

				for offset in neighbours {
					dx := current.x + offset.x
					dy := current.y + offset.y
					if index_in_bounds(dx, chart) &&
					   index_in_bounds(dy, line) &&
					   chart[dx][dy].value != 9 &&
					   !chart[dx][dy].visited {
						append(&cells_to_check, Vector2{dx, dy})
					}
				}
				chart[current.x][current.y].visited = true
			}
			append(&basins_sizes, current_basin_size)
		}
	}

	slice.reverse_sort(basins_sizes[:])
	three_largest := basins_sizes[0] * basins_sizes[1] * basins_sizes[2]

	return low_point_sum, three_largest
}

day08 :: proc(lines: []string) -> (part1: int, part2: int) {
	display_map :: struct {
		t, uv, wx, yz: u8,
	}

	display :: struct {
		numbers: []u8,
		output:  []u8,
	}

	map_letters :: proc(digit: string) -> (u: u8) {
		for r in "abcdefg" {
			if strings.contains_rune(digit, r) do u += 1
			u <<= 1
		}
		return
	}

	pop_count :: proc(a: u8) -> (count: int) {
		for i := 1; i < 256; i <<= 1 {
			if a & u8(i) != 0 do count += 1
		}
		return
	}

	sort_by_len :: proc(a, b: u8) -> bool {
		return pop_count(a) < pop_count(b)
	}

	displays: [dynamic]display
	unique_count, sum_of_outputs: int

	// parse
	for line, index in lines {
		split := strings.split(line, " | ")

		numbers_mapped := make([]u8, 10)
		output_mapped := make([]u8, 4)

		numbers_raw := strings.split(split[0], " ")
		output_raw := strings.split(split[1], " ")

		for d, i in output_raw do output_mapped[i] = map_letters(d)
		for d, i in numbers_raw do numbers_mapped[i] = map_letters(d)

		d: display
		d.output = output_mapped
		d.numbers = numbers_mapped

		append(&displays, d)
		// Count
		for digit in output_raw {
			switch len(digit) {
			case 2, 3, 4, 7:
				unique_count += 1
			}
		}
	}

	// solve
	for d in displays {
		using d
		slice.sort_by(numbers, sort_by_len)

		sol: [10]u8

		sol[1] = numbers[0]
		sol[7] = numbers[1]
		sol[4] = numbers[2]
		sol[8] = numbers[9]

		dm: display_map
		// 1 gives us u and v
		dm.uv = sol[1]
		// 7 gives us t
		dm.t = sol[7] &~ sol[1]
		// 4 gives us y and z
		dm.yz = sol[4] &~ sol[1]
		// the last two are left over
		dm.wx = (sol[8] &~ sol[7]) &~ sol[4]

		// for 2 3 5 we need to check u v x y
		for i in 3 ..< 6 {
			n := numbers[i]
			uv := pop_count(n & dm.uv)
			wx := pop_count(n & dm.wx)
			yz := pop_count(n & dm.yz)
			if wx == 2 do sol[2] = n
			else if uv == 2 do sol[3] = n
			else if yz == 2 do sol[5] = n
			else do assert(false, "didnt match 2, 3 or 5")
		}
		// for 0 6 9 we need to check u x z
		for i in 6 ..< 9 {
			n := numbers[i]
			uv := pop_count(n & dm.uv)
			wx := pop_count(n & dm.wx)
			yz := pop_count(n & dm.yz)
			if uv + wx == 4 do sol[0] = n
			else if wx + yz == 4 do sol[6] = n
			else if uv + yz == 4 do sol[9] = n
			else do assert(false, "didnt match 0, 6 or 9")
		}

		value := 0
		for o, index in output {
			for s, i in sol {
				if o == s {
					value += i
					break
				}
			}
			if index < len(output) - 1 do value *= 10
		}
		sum_of_outputs += value
	}

	return unique_count, sum_of_outputs
}

day07 :: proc(lines: []string) -> (part1: int, part2: int) {
	crabs := line_to_numbers(lines[0])

	min_pos, max_pos := 0xffff_ffff, 0
	for crab in crabs {
		min_pos = min(min_pos, crab)
		max_pos = max(max_pos, crab)
	}

	linear_costs := make([]int, max_pos + 1)
	exp_costs := make([]int, max_pos + 1)
	step_costs := make([]int, max_pos + 1)
	for _, index in step_costs {
		step_costs[index] = index
		if index != 0 do step_costs[index] += step_costs[index - 1]
	}
	for pos in min_pos ..= max_pos {
		for crab in crabs {
			distance := abs(pos - crab)
			linear_costs[pos] += distance
			exp_cost := step_costs[distance]
			exp_costs[pos] += exp_cost
		}
	}

	linear_min, exp_min := 0xffff_ffff, 0xffff_ffff
	for cost in linear_costs do linear_min = min(linear_min, cost)
	for cost in exp_costs do exp_min = min(exp_min, cost)

	return linear_min, exp_min
}

day06 :: proc(lines: []string) -> (part1: int, part2: int) {
	days_to_spawn :: 6
	days_to_first_spawn :: days_to_spawn + 2

	numbers := line_to_numbers(lines[0])
	fish: [days_to_first_spawn + 1]int
	for n in numbers {
		fish[n] += 1
	}

	for day in 1 ..= 80 {
		next_fish: [days_to_first_spawn + 1]int
		for count, age in fish {
			if age == 0 {
				next_fish[days_to_first_spawn] += count
				next_fish[days_to_spawn] += count
			} else do next_fish[age - 1] += count
		}
		fish = next_fish
	}
	sum_80, sum_256: int
	for count in fish do sum_80 += count

	for day in 81 ..= 256 {
		next_fish: [days_to_first_spawn + 1]int
		for count, age in fish {
			if age == 0 {
				next_fish[days_to_first_spawn] += count
				next_fish[days_to_spawn] += count
			} else do next_fish[age - 1] += count
		}
		fish = next_fish
	}

	for count in fish do sum_256 += count

	return sum_80, sum_256
}

day05 :: proc(lines: []string) -> (part1: int, part2: int) {

	direction :: enum {
		WEST,
		SOUTH,
		DIAG,
		INV_DIAG,
	}
	range :: struct {
		start:     Vector2,
		length:    int,
		direction: direction,
	}
	using direction
	ranges := make([]range, len(lines))
	for line, index in lines {
		abcd := strings.split(line, " -> ")
		ab := strings.split(abcd[0], ",")
		cd := strings.split(abcd[1], ",")
		a := strconv.atoi(ab[0])
		b := strconv.atoi(ab[1])
		c := strconv.atoi(cd[0])
		d := strconv.atoi(cd[1])
		using r: range
		if a == c {
			direction = SOUTH
			start = Vector2{a, min(b, d)}
			length = max(b, d) - start.y
		} else if b == d {
			direction = WEST
			start = Vector2{min(a, c), b}
			length = max(a, c) - start.x
		} else if a < c && b < d || a > c && b > d {
			direction = DIAG
			if a < c && b < d {
				start = Vector2{a, b}
			} else {
				start = Vector2{c, d}
			}
			length = abs(a - c)
		} else if a < c && b > d || a > c && b < d {
			direction = INV_DIAG
			if a < c && b > d {
				start = Vector2{a, b}
			} else {
				start = Vector2{c, d}
			}
			length = abs(a - c)
		} else {
			assert(false, "unreachable")
		}
		ranges[index] = r
	}

	dim :: 1000
	chart_no_diag := make([]int, dim * dim)
	chart_diag := make([]int, dim * dim)

	for r in ranges {
		using r
		col, row := start.x, start.y
		switch direction {
		case WEST:
			for i in 0 ..= length {
				chart_no_diag[(col + i) * dim + row] += 1
				chart_diag[(col + i) * dim + row] += 1
			}
		case SOUTH:
			for i in 0 ..= length {
				chart_no_diag[col * dim + row + i] += 1
				chart_diag[col * dim + row + i] += 1
			}
		case DIAG:
			for i in 0 ..= length {
				chart_diag[(col + i) * dim + row + i] += 1
			}
		case INV_DIAG:
			for i in 0 ..= length {
				chart_diag[(col + i) * dim + row - i] += 1
			}
		}
	}

	no_diag, diag: int
	for cell in chart_no_diag do if cell > 1 do no_diag += 1
	for cell in chart_diag do if cell > 1 do diag += 1

	return no_diag, diag
}

day04 :: proc(lines: []string) -> (part1: int, part2: int) {

	board :: struct {
		nums:   [5][5]int,
		marked: [5][5]bool,
		won:    bool,
	}


	boards: [dynamic]board
	draws: []int
	numbers := strings.split(lines[0], ",")
	draws = make([]int, len(numbers))
	for it, index in numbers {
		draws[index] = strconv.atoi(it)
	}

	current: board
	row := 0
	can_append := false
	for line in lines[1:] {
		if len(line) == 0 {
			if !can_append do can_append = true
			else do append(&boards, current)
			current = {}
			row = 0
			continue
		}
		cells := strings.split(line, " ")
		column := 0
		for it in cells {
			if it == "" do continue

			current.nums[row][column] = strconv.atoi(it)
			column += 1
		}
		row += 1
	}
	append(&boards, current)
	// simulate the game
	size :: 5

	result :: struct {
		found: bool,
		board: board,
		draw:  int,
	}
	winning: result
	losing: result

	simulate: for draw in draws {
		// mark draw in all boards
		for _, i in boards {
			search: for row in 0 ..< size {
				for col in 0 ..< size {
					if boards[i].nums[row][col] == draw {
						boards[i].marked[row][col] = true
						break search
					}
				}
			}
		}
		// check all boards for a win
		for board, index in boards {
			using board
			rows: for row in 0 ..< size {
				for col in 0 ..< size {
					if !marked[row][col] do continue rows
				}
				if !winning.found {
					winning.found = true
					winning.board = board
					winning.draw = draw
				}
				if len(boards) == 1 {
					losing.found = true
					losing.board = board
					losing.draw = draw
				}
				boards[index].won = true
				continue
			}

			cols: for col in 0 ..< size {
				for row in 0 ..< size {
					if !marked[row][col] do continue cols
				}
				if !winning.found {
					winning.found = true
					winning.board = board
					winning.draw = draw
				}
				if len(boards) == 1 {
					losing.found = true
					losing.board = board
					losing.draw = draw
				}
				boards[index].won = true

				continue
			}
		}
		#reverse for board, index in boards {
			if board.won do ordered_remove(&boards, index)
		}
	}

	sum_unmarked :: proc(using b: board) -> int {
		sum: int
		for row in 0 ..< size {
			for col in 0 ..< size {
				if !marked[row][col] do sum += nums[row][col]
			}
		}
		return sum
	}


	return winning.draw * sum_unmarked(winning.board), losing.draw * sum_unmarked(losing.board)
}

day03 :: proc(lines: []string) -> (part1: int, part2: int) {

	pop_count :: proc(lines: []string, column: int) -> bool {
		ones: int
		for line in lines do if line[column] == '1' do ones += 1

		if len(lines) % 2 == 1 do return ones > len(lines) / 2
		else do return ones >= len(lines) / 2
	}

	columns := len(lines[0])

	gamma_rate: int
	epsilon_rate: int
	{
		for column in 0 ..< columns {
			gamma_rate <<= 1
			if pop_count(lines, column) do gamma_rate += 1
		}

		mask: int
		for i in 0 ..< columns do mask = (mask << 1) | 1

		epsilon_rate = ~gamma_rate & mask
	}

	oxygen, co2: int
	{
		ox_lines: [dynamic]string

		for line in lines do append(&ox_lines, line)
		for column in 0 ..< columns {
			pop_is_one := pop_count(ox_lines[:], column)
			#reverse for line, index in ox_lines {
				col_is_one := line[column] == '1'
				if pop_is_one ~ col_is_one do unordered_remove(&ox_lines, index)
			}

			if len(ox_lines) == 1 do break
		}

		co2_lines: [dynamic]string
		for line in lines do append(&co2_lines, line)
		for column in 0 ..< columns {
			pop_is_one := pop_count(co2_lines[:], column)
			#reverse for line, index in co2_lines {
				col_is_one := line[column] == '1'
				if pop_is_one == col_is_one do ordered_remove(&co2_lines, index)
			}

			if len(co2_lines) == 1 do break
		}

		ok: bool
		oxygen, ok = strconv.parse_int(ox_lines[0], 2)
		if !ok do return 0, 0

		co2, ok = strconv.parse_int(co2_lines[0], 2)
		if !ok do return 0, 0
	}

	return gamma_rate * epsilon_rate, oxygen * co2
}

day02 :: proc(lines: []string) -> (part1: int, part2: int) {
	basic_position := Vector2{0, 0}
	aimed_position := Vector2{0, 0}
	aim := 0
	for line in lines {
		parts := strings.split(line, " ")
		command, number := parts[0], parts[1]
		value, ok := strconv.parse_int(number)
		if !ok do return

		if strings.has_prefix(command, "f") {
			basic_position += Vector2{value, 0}
			aimed_position += Vector2{value, aim * value}

		} else if strings.has_prefix(command, "d") {
			basic_position += Vector2{0, value}
			aim += value
		} else if strings.has_prefix(command, "u") {
			basic_position -= Vector2{0, value}
			aim -= value
		}
	}

	return basic_position.x * basic_position.y, aimed_position.x * aimed_position.y
}

day01 :: proc(lines: []string) -> (part1: int, part2: int) {
	nums := make([]int, len(lines))

	for line, index in lines {
		n, ok := strconv.parse_int(line, base = 10)
		if !ok do return
		nums[index] = n
	}

	single_increases := 0
	window_increases := 0
	for i in 1 ..< len(nums) {
		if nums[i] > nums[i - 1] do single_increases += 1

		if i < 3 do continue

		if nums[i] > nums[i - 3] do window_increases += 1
	}
	return single_increases, window_increases
}
