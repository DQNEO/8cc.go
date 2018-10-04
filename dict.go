package main

type Dict struct {
	list   []*DictEntry
	parent *Dict
}

type DictValue struct {
	ast   *Ast
	ctype *Ctype
	tok   *Token
}

type DictEntry struct {
	key string
	val *DictValue
}

func MakeDict(parent *Dict) *Dict {
	return &Dict{
		parent: parent,
	}
}

func (dict *Dict) Parent() *Dict {
	return dict.parent
}

func (dict *Dict) GetAst(key string) *Ast {
	val := dict.Get(key)
	if val == nil {
		return nil
	}
	return val.ast
}

func (dict *Dict) GetCtype(key string) *Ctype {
	val := dict.Get(key)
	if val == nil {
		return nil
	}
	return val.ctype
}

func (dict *Dict) GetToken(key string) *Token {
	val := dict.Get(key)
	if val == nil {
		return nil
	}
	return val.tok
}

func (dict *Dict) Get(key string) *DictValue {
	for d := dict; d != nil; d = d.parent {
		for _, e := range d.list {
			if e.key == key {
				return e.val
			}
		}
	}
	return nil
}

func (dict *Dict) PutAst(key string, val *Ast) {
	dict.Put(key, &DictValue{
		ast: val,
	})
}

func (dict *Dict) PutCtype(key string, val *Ctype) {
	dict.Put(key, &DictValue{
		ctype: val,
	})
}

func (dict *Dict) PutToken(key string, val *Token) {
	dict.Put(key, &DictValue{
		tok: val,
	})
}

func (dict *Dict) Put(key string, val *DictValue) {
	e := &DictEntry{
		key: key,
		val: val,
	}
	dict.list = append(dict.list, e)
}

func (dict *Dict) Empty() bool {
	return len(dict.list) == 0
}

func (dict *Dict) Keys() []string {
	var r []string
	for d := dict; d != nil; d = d.parent {
		for _, e := range d.list {
			r = append(r, e.key)
		}
	}
	return r
}

func (dict *Dict) Values() []*DictValue {
	var r []*DictValue
	for d := dict; d != nil; d = d.parent {
		for _, e := range d.list {
			r = append(r, e.val)
		}
	}
	return r
}
