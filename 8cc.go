package main

import "fmt"

func main() {
	var val int
	_, err := fmt.Scanf("%d", &val)
	if err != nil {
		panic("scanf")
	}
	fmt.Printf("\t.text\n\t"+
		".global mymain\n"+
		"mymain: \n\t"+
		"mov $%d, %%eax\n\t"+
		"ret\n", val)
}
