package main

type TokenList []*Token

func list_pop(list TokenList) (TokenList, *Token) {
	if len(list) == 0 {
		return list, nil
	}
	tok := list[len(list)-1]
	list = list[:len(list)-1]
	return list, tok
}

func list_append(a TokenList, b TokenList) TokenList{
	r := a
	for _, tok := range b {
		r = append(r, tok)
	}
	return r
}

func list_reverse(a TokenList) TokenList {
	for i := len(a)/2-1; i >= 0; i-- {
		opp := len(a)-1-i
		a[i], a[opp] = a[opp], a[i]
	}
	return a
}
