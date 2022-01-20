let first (xs : 'a list) : 'a = List.nth xs 0
let second (xs : 'a list) : 'a = List.nth xs 1
let third (xs : 'a list) : 'a = List.nth xs 2
let fourth (xs : 'a list) : 'a = List.nth xs 3
let fifth (xs : 'a list) : 'a = List.nth xs 4
let sixth (xs : 'a list) : 'a = List.nth xs 5
let seventh (xs : 'a list) : 'a = List.nth xs 6
let eighth (xs : 'a list) : 'a = List.nth xs 7
let ninth (xs : 'a list) : 'a = List.nth xs 8

let rest (xs : 'a list) : 'a list = List.tl xs
