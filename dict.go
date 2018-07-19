package main

type DictCtype []*DictCtypeEntry

type DictCtypeEntry struct {
	key string
	val *Ctype
}

func NewDictCtype() *DictCtype {
	return &DictCtype{}
}

func (dict *DictCtype) Get(key string) *Ctype {
	for _, e := range *dict {
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
	*dict = append(*dict, e)
}

func (dict *DictCtype) Keys() []string {
	var r []string
	for _, e := range *dict {
		r = append(r, e.key)
	}
	return r
}

func (dict *DictCtype) Values() []*Ctype {
	var r []*Ctype
	for _, e := range *dict {
		r = append(r, e.val)
	}
	return r
}

