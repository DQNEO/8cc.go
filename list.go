package main

type TokenList []*Token

func make_list() TokenList {
	return make(TokenList, 0)
}

func list_pop(list TokenList) (TokenList, *Token) {
	if len(list) == 0 {
		return list, nil
	}
	tok := list[len(list)-1]
	list = list[:len(list)-1]
	return list, tok
}

func list_append(a TokenList, b TokenList) TokenList {
	r := a
	for _, tok := range b {
		r = append(r, tok)
	}
	return r
}

func list_reverse(a TokenList) TokenList {
	b := make(TokenList, len(a))
	for i := 0; i < len(a); i++ {
		b[len(b)-1-i] = a[i]
	}
	return b
}
