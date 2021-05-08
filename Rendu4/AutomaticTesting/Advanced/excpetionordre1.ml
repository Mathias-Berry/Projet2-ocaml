try (raise (E 5), raise (E 4), raise (E 3)) with | E k -> (1, 2, prInt k)
