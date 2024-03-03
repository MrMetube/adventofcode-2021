package main

import "core:fmt"
import "core:strings"
import "core:strconv"
import "core:os"


Vector2 :: [2]int


print_bits :: proc(a: u8) {
	for i:=256; i>0; i >>= 1{
		if a & u8(i) != 0  do fmt.print('1')
		else do fmt.print('0')
	}
	fmt.println()
}

line_to_numbers :: proc(line:string, separator := ",") -> ([]int) {
	numbers := strings.split(line, separator)
	result := make([]int, len(numbers))
	for n, i in numbers {
		result[i] = strconv.atoi(n)
	}
	return result
}

read_lines :: proc(file: string) -> (result: []string, success: bool) {
	data, ok := os.read_entire_file(file)
	if !ok do return nil, false
	return strings.split_lines(string(data)), true
}

index_in_bounds :: #force_inline proc "contextless" (index: int, array:[]$T) -> bool {
	return index >= 0 && index < len(array)
}
