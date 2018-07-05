package main

type DictCtype []*DictCtypeEntry

type DictCtypeEntry struct {
	key string
	val *Ctype
}

func dict_ctype_get(dict DictCtype, key string) *Ctype {
	for _, e := range dict {
		if e.key == key {
			return e.val
		}
	}
	return nil
}

func dict_ctype_put(dict *DictCtype, key string, val *Ctype)  {
	e := &DictCtypeEntry{
		key: key,
		val: val,
	}
	*dict = append(*dict, e)
}

func dict_ctype_keys(dict DictCtype) []string {
	var r []string
	for _, e := range dict {
		r = append(r, e.key)
	}
	return r
}

func dic_ctype_values(dict DictCtype) []*Ctype {
	var r []*Ctype
	for _, e := range dict {
		r = append(r, e.val)
	}
	return r
}

