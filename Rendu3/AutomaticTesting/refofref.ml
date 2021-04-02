let a = ref [1;2;3] in let b = ref a in !b := 3 in prInt !a
