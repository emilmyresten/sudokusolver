type cell_content = Domain of int list | Guess of int | Final of int | Init

let guess_count_ref = ref 0

let inc a = a + 1

let dec a = a - 1

let rec range m n = match m with
  | _ when m = n -> []
  | _ -> m :: if m > n then range (dec m) n else range (inc m) n


let rec range_with_content m n content = match m with
  | _ when m = n -> [content]
  | _ -> content :: if m > n then range_with_content (dec m) n content else range_with_content (inc m) n content


let board_template rows cols =
	let row = range_with_content 1 cols Init
	in let rec build_board board rows_to_add =
		if rows_to_add = 0 then board else row :: build_board board (dec rows_to_add)
	in build_board [] rows

let rec update_in lst i content = match lst with 
	| [] -> []
	| h :: t when i < 0 -> h :: t
	| h :: t when i > List.length lst -> h :: t
	| h :: t -> if i = 0 then content :: t else h :: update_in t (dec i) content

let rec update_in_board row col content board = match board with
	| [] -> [] (* this is the end of the list *)
	| h :: t when row = 0 -> update_in h col content :: t
	| h :: t -> h :: update_in_board (dec row) col content t


let example_sudoku =
	let () = Random.init 0 in 
	let init_board = board_template 9 9 in 
	let solvable_sudoku_values = [(0, 1, 2); (0, 3, 5); (0, 5, 1); (0, 7, 9);
						 (1, 0, 8); (1, 3, 2); (1, 5, 3); (1, 8, 6);
						 (2, 1, 3); (2, 4, 6); (2, 7, 7);
						 (3, 2, 1); (3, 6, 6);
						 (4, 0, 5); (4, 1, 4); (4, 7, 1); (4, 8, 9);
						 (5, 2, 2); (5, 6, 7);
						 (6, 1, 9); (6, 4, 3); (6, 7, 8);
						 (7, 0, 2); (7, 3, 8); (7, 5, 4); (7, 8, 7);
						 (8, 1, 1); (8, 3, 9); (8, 5, 7); (8, 7, 6);] in
	let rec initialize_board board rcv = match rcv with
		| [] -> board
		| (r, c, v) :: t -> initialize_board (update_in_board r c (Final v) board) t
	in initialize_board init_board solvable_sudoku_values

let rec get_row row_number board = match board with
	| [] -> [] (*only when initial list empty*)
	| row :: rows -> if row_number = 0 then row else get_row (dec row_number) rows

let rec get_in row i = match row with 
	| [] -> None
	| col :: cols -> if i = 0 then Some col else get_in cols (dec i) 

let rec get_in_board row_number col_number board = match board with
	| [] -> None (* this is the end of the list *)
	| h :: _ when row_number = 0 ->
		(match get_in h col_number with 
	 		| x -> x)
	| _ :: t -> get_in_board (dec row_number) col_number t

let rec get_col col_number board = match board with
	| [] -> []
	| row :: rows -> 
		match (get_in row col_number) with
			| None -> get_col col_number rows
			| Some v -> v :: get_col col_number rows


let rec values_in_row row = match row with
	| [] -> []
	| Final v :: t -> v :: values_in_row t
	| Guess v :: t -> v :: values_in_row t
	| _ :: t -> values_in_row t

let rec values_in_col col = match col with
	| [] -> []
	| Final v :: t -> v :: values_in_col t
	| Guess v :: t -> v :: values_in_col t
	| _ :: t -> values_in_col t

(** Region 1: row 0-2, col 0-2
	Region 2: row 0-2, col 3-5
	Region 3: row 0-2, col 6-8
	Region 4: row 3-5, col 0-2
	Region 5: row 3-5, col 3-5
	Region 6: row 3-5, col 6-8
	Region 7: row 6-8, col 0-2
	Region 8: row 6-8, col 3-5
	Region 9: row 6-8, col 6-8*)
let get_region_indexes row_number col_number = match (row_number, col_number) with

		| (r, c) when r < 3 && c < 3 -> [(0, 0); (0, 1); (0, 2);
										 (1, 0); (1, 1); (1, 2);
										 (2, 0); (2, 1); (2, 2);]
		| (r, c) when r < 3 && c < 6 -> [(0, 3); (0, 4); (0, 5);
										 (1, 3); (1, 4); (1, 5);
										 (2, 3); (2, 4); (2, 5);]
		| (r, c) when r < 3 && c < 9 -> [(0, 6); (0, 7); (0, 8);
										 (1, 6); (1, 7); (1, 8);
										 (2, 6); (2, 7); (2, 8);]

		| (r, c) when r < 6 && c < 3 -> [(3, 0); (3, 1); (3, 2);
										 (4, 0); (4, 1); (4, 2);
										 (5, 0); (5, 1); (5, 2);]
		| (r, c) when r < 6 && c < 6 -> [(3, 3); (3, 4); (3, 5);
										 (4, 3); (4, 4); (4, 5);
										 (5, 3); (5, 4); (5, 5);]
		| (r, c) when r < 6 && c < 9 -> [(3, 6); (3, 7); (3, 8);
										 (4, 6); (4, 7); (4, 8);
										 (5, 6); (5, 7); (5, 8);]

		| (r, c) when r < 9 && c < 3 -> [(6, 0); (6, 1); (6, 2);
										 (7, 0); (7, 1); (7, 2);
										 (8, 0); (8, 1); (8, 2);]
		| (r, c) when r < 9 && c < 6 -> [(6, 3); (6, 4); (6, 5);
										 (7, 3); (7, 4); (7, 5);
										 (8, 3); (8, 4); (8, 5);]
		| (r, c) when r < 9 && c < 9 -> [(6, 6); (6, 7); (6, 8);
										 (7, 6); (7, 7); (7, 8);
										 (8, 6); (8, 7); (8, 8);]

		| _ -> 	(*should never happen*)	[(0, 0); (0, 0); (0, 0);
										 (0, 0); (0, 0); (0, 0);
										 (0, 0); (0, 0); (0, 0);]

(** this function assumes a 9x9 grid with 3x3 regions. 
	It is, in other words, hard-coded for simplicities sake. *)
let get_region row_number col_number board = 
	let region_indexes = get_region_indexes row_number col_number in
	let rec aux indexes = match indexes with
		| [] -> []
		| h :: t -> (match (get_in_board (fst h) (snd h) board) with
						| None -> []
						| Some v -> v :: aux t)
	in aux region_indexes

let rec values_in_region region = match region with
	| [] -> []
	| Final v :: t -> v :: values_in_region t
	| Guess v :: t -> v :: values_in_region t
	| _ :: t -> values_in_region t

let find_domain_for_cell row_number col_number board = 
	(* Printf.printf "find domain for: %d, %d" row_number col_number; *)
	let potential_domain = range 1 10 in
	let disallowed = 
		values_in_row (get_row row_number board) @ 
		values_in_col (get_col col_number board) @ 
		values_in_region (get_region row_number col_number board) in
	List.filter (fun x -> not (List.mem x disallowed)) potential_domain

let traverse_board_and_do fn full_board =
	let rows = List.length full_board and cols = List.length (List.hd full_board) in
	let rec aux r c board = match (get_in_board r c board) with
		| Some cell -> aux r (inc c) (fn cell (r, c) board) (* fn does something to the board, and returns a new board. *)	
		| _ when r = rows -> board (* finish when we're past the last row *)
		| _ when c = cols -> aux (inc r) 0 board (* handle wrap around *)
		| _ -> board (* should never happen. *)
	in aux 0 0 full_board
	
let update_domains full_board = 
	let set_domain = (fun cell coords remaining_board -> 
		let row = fst coords and col = snd coords in
		match cell with
			| Final _ | Guess _ -> remaining_board
			| _ -> update_in_board row col (Domain (find_domain_for_cell row col full_board)) remaining_board)
	in traverse_board_and_do set_domain full_board


let finalize_single_domains full_board =
	let make_final = (fun cell coords remaining_board ->
		let row = fst coords and col = snd coords in
		match cell with
			| Domain (h :: []) -> update_in_board row col (Final h) remaining_board
			| _ -> remaining_board)
	in traverse_board_and_do make_final full_board


let print_board board = 
	let cols = List.length (List.hd board) in
	let printer cell coords remaining_board = (match cell with 
		| Init when snd coords = dec cols -> Format.printf "|    |\n"; remaining_board
		| Domain _ when snd coords = dec cols -> Format.printf "|  D  |\n"; remaining_board
		| Final v when snd coords = dec cols -> Format.printf "|  %d  |\n" v; remaining_board
		| Guess v when snd coords = dec cols -> Format.printf "| G%d  |\n" v; remaining_board
		| Init -> Format.printf "|    "; remaining_board
		| Domain _ -> Format.printf "| D  "; remaining_board
		| Final v -> Format.printf "|  %d " v; remaining_board
		| Guess v -> Format.printf "| G%d " v; remaining_board)
	in traverse_board_and_do printer board

let finalize_guesses full_board =
	let finalize_guess = (fun cell coords remaining_board ->
		let row = fst coords and col = snd coords in
		match cell with
			| Guess v -> update_in_board row col (Final v) remaining_board
			| _ -> remaining_board) 
	in 
	traverse_board_and_do finalize_guess full_board 

let is_final full_board =
	let rows = List.length full_board and cols = List.length (List.hd full_board) in
	let rec aux r c board = match (get_in_board r c board) with
		| Some (Domain _ | Guess _ | Init) -> false
		| _ when r = rows -> true (* finish when we're past the last row *)
		| _ when c = cols -> aux (inc r) 0 board (* handle wrap around *)
		| _ -> aux r (inc c) board (* should never happen. *)
	in aux 0 0 full_board

let make_guess r c guess board = 
	guess_count_ref := inc !guess_count_ref;
	update_in_board r c (Guess guess) board
	|> update_domains

let solve sudoko =
	let start_row = 0
	and start_col = 0 
	and rows = List.length sudoko 
	and cols = List.length (List.hd sudoko) in 
	let rec solver r c board = 
		let _ = print_board board in
		match (get_in_board r c board) with
		| Some cell -> 
			(match cell with 
			| Domain d -> 
				(let rec guess domain guess_board = match domain with
					| [] -> guess_board
					| h :: t -> 
						let new_board = 
							make_guess r c h guess_board
							|> solver r (inc c) 
							(* want to check if I managed to make a guess on the next choice, otherwise retry with guess from t. 
							   Need to implement backtracking. *)
						in
						if r = dec rows && c = dec cols then finalize_guesses new_board
						else if is_final new_board then new_board else guess t guess_board
				in guess d board)
			| _ -> solver r (inc c) board) (* skip guesses or final. *)
		| _ when r = rows -> board
		| _ when c = cols -> solver (inc r) 0 board
		| _ -> board (* should never happen *)
	in
	let _ = update_domains sudoko |> solver start_row start_col |> print_board
	in Format.printf "Found solution in %d guesses" !guess_count_ref; 
		 guess_count_ref := 0;