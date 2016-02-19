(* CALCULATIONS *)
let t8a = has_vars (Add (Var, Int 2))
let t8b = not (has_vars (Add (Int 1, Int 2)))
let t8c = has_vars (Add (Var, Var))
let t8d = has_vars (Mul (Var, Var))
let t8e = has_vars (Sub (Var, Var))
let t8f = has_vars (Parity (Var))
let t8g = has_vars (Var)
let t8h = not (has_vars (Sub (Int 2, Int 2)))
let t8i = not (has_vars (Mul (Int 7, Int 9)))
let t8j = not (has_vars (Parity (Int 5)))
let t8k = not (has_vars (Int 4))


let t9a = count_vars (Add (Var, Int 2)) = 1
let t9b = count_vars (Add (Int 1, Int 2)) = 0
let t9c = count_vars (Add (Var, Var)) = 2
let t9d = count_vars (Mul (Int 1, Int 5)) = 0
let t9e = count_vars (Mul (Int 5, Var)) = 1
let t9f = count_vars (Mul (Var, Var)) = 2
let t9g = count_vars (Sub (Int 2, Int 4)) = 0
let t9h = count_vars (Sub (Var, Int 2)) = 1
let t9i = count_vars (Sub (Var, Var)) = 2
let t9j = count_vars (Parity (Int 2)) = 0
let t9k = count_vars (Parity (Var)) = 1
let t9l = count_vars (Var) = 1
let t9m = count_vars (Int 7) = 0


let t10a = calc_eval (Add (Var, Int 2), 3) = 5
let t10b = calc_eval (Add (Int 8, Int 2), 0) = 10
let t10c = calc_eval (Add (Var, Int 4), 4) = 8
let t10d = calc_eval (Sub (Var, Int 7), 9) = 2
let t10e = calc_eval (Sub (Int 5, Var), 6) = -1
let t10f = calc_eval (Mul (Var, Int 2), 5) = 10
let t10g = calc_eval (Mul (Int 8, Int 4), 0) = 32
let t10h = calc_eval (Parity (Var), 3) = 1
let t10i = calc_eval (Parity (Int 2), 3) = 0
let t10j = calc_eval (Int 4, 5) = 4
let t10k = calc_eval (Var, 8) = 8


let t11a = func_of_calc (Add (Var, Int 2)) 3 = 5
let t11b = func_of_calc (Add (Int 2, Int 2)) 3 = 4
let t11c = func_of_calc (Sub (Int 9, Var)) 4 = 5
let t11d = func_of_calc (Sub (Int 5, Int 3)) 8 = 2
let t11e = func_of_calc (Mul (Var, Int 2)) 2 = 4
let t11f = func_of_calc (Mul (Var, Var)) 5 = 25
let t11g = func_of_calc (Parity (Var)) 3 = 1
let t11h = func_of_calc (Parity (Int 2)) 3 = 0
let t11i = func_of_calc (Int 8) 4 = 8
let t11j = func_of_calc (Var) 5 = 5


let t12a = subst (Add (Var, Int 1), Mul (Var, Var)) = Mul (Add (Var, Int 1), Add (Var, Int 1))
let t12b = subst (Mul (Var, Int 2), Add (Var, Var)) = Add (Mul (Var, Int 2), Mul (Var, Int 2))
let t12c = subst (Int 2, Mul (Var, Var)) = Mul (Int 2, Int 2)
let t12d = subst (Int 2, Var) = Int 2
let t12e = subst (Sub (Int 8, Var), Mul (Var, Var)) = Mul (Sub (Int 8, Var), Sub (Int 8, Var))


let t13a = power (Var, 3) = Mul (Mul (Var, Var), Var)
let t13b = power (Var, 0) = Int 1
let t13c = power (Var, 4) = Mul (Mul (Mul (Var, Var), Var), Var)
let t13d = power (Var, 1) = Var


let t14a = term (2, 1) = Mul (Int 2, Var)
let t14b = term (0, 4) = Mul (Int 0, Mul (Mul (Mul (Var, Var), Var), Var))
let t14c = term (1, 1) = Mul (Int 1, Var)
let t14d = term (2, 0) = Mul (Int 2, Int 1)


let t15a = poly [(2, 1); (1, 4)] = Add (term (2, 1), term (1, 4))
let t15b = poly [(2, 1); (0, 2); (1, 4)] = Add (term (2, 1), term (1, 4))
let t15c = poly [(0, 1); (0, 4)] = Int 0
let t15d = poly [(0, 1)] = Int 0
let t15e = poly [] = Int 0
let t15f = poly [(2, 1); (0, 1)] = term (2, 1)
let t15g = poly [(0, 1); (2, 1)] = term (2, 1)


let t16a = simplify (Add (Int 0, Var)) = Var
let t16b = simplify (Add (Int 3, Int 4)) = Int 7
let t16c = calc_eval (simplify (poly [(2, 1); (1, 0)]), 3) = 7

