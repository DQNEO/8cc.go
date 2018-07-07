package main

type DictAst struct {
	list []*DictAstEntry
	parent *DictAst
}
type DictCtype struct {
	list []*DictCtypeEntry
	parent *DictCtype
}

type DictAstEntry struct {
	key string
	val *Ast
}

func NewDictAst() *DictAst {
	return &DictAst{
	}
}

func (dict *DictAst) MakeDict() *DictAst {
	r := &DictAst{
		parent: dict,
	}
	return r
}

func (dict *DictAst) Parent() *DictAst {
	return dict.parent
}

func (dict *DictAst) Get(key string) *Ast {
	for d := dict;d != nil; d = d.parent {
		for _, e := range d.list {
			if e.key == key {
				return e.val
			}
		}
	}
	return nil
}

func (dict *DictAst) Put(key string, val *Ast)  {
	e := &DictAstEntry{
		key: key,
		val: val,
	}
	dict.list = append(dict.list, e)
}

func (dict *DictAst) Keys() []string {
	var r []string
	for d := dict;d != nil; d = d.parent {
		for _, e := range d.list {
			r = append(r, e.key)
		}
	}
	return r
}

func (dict *DictAst) Values() []*Ast {
	var r []*Ast
	for d := dict;d != nil; d = d.parent {
		for _, e := range d.list {
			r = append(r, e.val)
		}
	}
	return r
}

type DictCtypeEntry struct {
	key string
	val *Ctype
}

func NewDictCtype() *DictCtype {
	return &DictCtype{}
}

func (dict *DictCtype) Get(key string) *Ctype {
	for _, e := range dict.list {
		if e.key == key {
			return e.val
		}
	}
	return nil
}

func (dict *DictCtype) Put(key string, val *Ctype)  {
	e := &DictCtypeEntry{
		key: key,
		val: val,
	}
	dict.list = append(dict.list, e)
}

func (dict *DictCtype) Keys() []string {
	var r []string
	for _, e := range dict.list {
		r = append(r, e.key)
	}
	return r
}

func (dict *DictCtype) Values() []*Ctype {
	var r []*Ctype
	for _, e := range dict.list {
		r = append(r, e.val)
	}
	return r
}

