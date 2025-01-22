# %% [markdown]
# # 4. domača naloga

# %% [markdown]
# Pri tej nalogi boste napisali svoj simulator Turingovih strojev. Zaradi preprostosti bomo za abecedo vzeli kar znake tipa `char`, za prazni znak bomo izbrali presledek `' '`, stanja pa bomo predstavili z nizi. Za možne premike zafiksiramo tip `direction`:

# %%
type direction =
    | Left
    | Right

type state = string

# %% [markdown]
# ## Implementacija trakov

# %% [markdown]
# Napišite modul `Tape`, ki implementira spodnjo signaturo, kjer je:
#
# - `t` tip v obe smeri neomejenih trakov in glavo na danem mestu;
# - `make`, ki naredi nov trak z znaki iz niza ter glavo na prvem znaku;
# - `read`, ki vrne znak pod glavo;
# - `write`, ki pod glavo zapiše dani znak;
# - `move`, ki glavo premakne v dano smer;
# - `print`, ki izpiše vsebino traku (brez presledkov na začetku in koncu) ter pod njim z `^` označi mesto glave.
#
# Zadnji dve funkciji naj vrneta nov trak, obstoječega pa naj pustita nespremenjenega.
#
# Ker je tip `t` abstrakten, si lahko privoščite poljubno implementacijo, zato poskrbite tako za učinkovitost kot za preglednost kode.

# %%
module type TAPE =
    sig
        type t

        val make : string -> t
        val move : direction -> t -> t
        val read : t -> char
        val write : char -> t -> t
        val print : t -> unit
    end

# %%
module Tape : TAPE =
    struct
        (* Choosing the correct type has an impact on time efficiency. We can limit ourselves as follows: *)

        (* -`make` function has to read every character in the string so at best it is O(n) *)
        (* -`move` function has to move the tape by a single step so it should be O(1) *)
        (* -`read` function has to output a single character so it should be O(1) *)
        (* -`write` function has to replace a single character so it should be O(1) *)
        (* -`print` function has to read the entire tape so at best it is O(n) *)

        (* We see that our goal should be optimizing `move`, `read` and `write`, since the others are at least O(n). *)
        (* This means we need O(1) access to reading and writing of the head and O(1) to the characters next to it. *)

        (* Idea: Store the head seperately for easy reading and writing and have lists of characters to the *)
        (* left and right with the first character being the one next to the head *)

        type t =
            {
                left : char list;
                head : char;
                right : char list
            }

        let empty_tape : t =
            { left = []; head = ' '; right = [] }

        let make (s : string) : t =
            let str_to_list (s : string) : char list =
                List.init (String.length s) (String.get s)
            in
            match str_to_list (String.trim s) with
            | [] -> empty_tape
            | hd :: tl -> { empty_tape with head = hd; right = tl }

        let move (dir : direction) (tape : t) : t =
            let order (hd : char) ((dir_chars, other_chars) : char list * char list) : t =
                match dir with
                | Left -> { left = dir_chars; head = hd; right = other_chars }
                | Right -> { left = other_chars; head = hd; right = dir_chars }
            and trim_others (other_chars : char list) : char list =
                if tape.head = ' ' && other_chars = [] then other_chars
                else tape.head :: other_chars
            and (dir_chars, other_chars) : char list * char list =
                match dir with
                | Left -> (tape.left, tape.right)
                | Right -> (tape.right, tape.left)
            in
            match dir_chars with
            | [] -> order ' ' ([], trim_others other_chars)
            | hd' :: dir_chars' -> order hd' (dir_chars', trim_others other_chars)

        let read (tape : t) : char =
            tape.head

        let write (c : char) (tape : t) : t =
            { tape with head = c }

        let print (tape : t) : unit =
            List.iter print_char (List.rev tape.left);
            print_char tape.head;
            List.iter print_char tape.right;
            print_newline ();
            print_string (String.make (List.length tape.left) ' ');
            print_char '^';
            print_newline ();
    end

# %%
let primer_trak : unit =
    Tape.(
        make "ABCDE"
        |> move Left
        |> move Left
        |> move Right
        |> move Right
        |> move Right
        |> move Right
        |> write '!'
        |> print
    )

# %%
let primer_trak_dodatno : unit =
    Tape.(
        make "ABC"
        |> move Left
        |> move Left
        |> write '!'
        |> move Right
        |> move Right
        |> print
    )

# %% [markdown]
# ## Implementacija Turingovih strojev

# %% [markdown]
# Napišite modul `Machine`, ki implementira spodnjo signaturo, kjer je:
#
# - `t` tip Turingovih strojev;
# - `make`, ki naredi nov stroj z danim začetnim stanjem in seznamom preostalih stanj ter prazno prehodno funkcijo;
# - `initial`, ki vrne začetno stanje stroja;
# - `add_transition`, ki prehodno funkcijo razširi s prehodom $(q, a) \mapsto (q', a', d)$;
# - `step`, ki za dano stanje in trak izvede en korak stroja, če je to mogoče.
#
# Zadnji dve funkciji naj vrneta spremenjene vrednosti, obstoječe argumente pa naj pustita nespremenjene. Prav tako pri zadnjih dveh funkcijah lahko predpostavite, da ju bomo klicali le na poprej podanih stanjih.

# Tudi tu je tip `t` abstrakten, zato poskrbite za učinkovitost in preglednost kode.

# %%
module type MACHINE =
    sig
        type t

        val make : state -> state list -> t
        val initial : t -> state
        val add_transition : state -> char -> state -> char -> direction -> t -> t
        val step : t -> state -> Tape.t -> (state * Tape.t) option
    end

# %%
module type MACHINE_EXTRA =
    sig
        type t

        val make : state -> state list -> t
        val initial : t -> state
        val add_transition : state -> char -> state -> char -> direction -> t -> t
        val step : t -> state -> Tape.t -> (state * Tape.t) option

        (* Extra exposed values enabling faster Machine execution *)

        type state_id

        val initial_id : t -> state_id
        val setup : t -> t
        val speed_step : t -> state_id -> Tape.t -> (state_id * Tape.t) option
    end

# %%
(* WARNING!!! *)
(* This implementation is only for showcasing a simpler but slightly less efficient solution. *)
(* Most of the time this would be more than fast enough and due to its much cleaner code and readability it might be the superior option. *)
(* Nontheless it is incompatible with the current implementation of `slow_run` and `speed_run` *)
module Machine : MACHINE =
    struct
        (* Turing Machine implementation using binary search trees for representing the transition function: *)

        (* -`make` function sets saves the inital state and given states and sets up an empty transition map - O(1) *)
        (* -`initial` function just returns the stored initial state - O(1) *)
        (* -`add_transition` function adds a mapping to the transition map - O(log n) *)
        (* -`step` function returns the resulting next state and tape calculated from the given current state and tape. *)
        (*        operations on the tape are executed in O(1) time and searching the transition map takes O(log n) - O(log n) *)

        (* This implementation is easy to use and is relatively efficient, but making the `step` function O(1) is more important than *)
        (* optimizing `make` and `add_transition`. Therefore other solutions can yield better results. *)

        type t =
            {
                initial : state;
                state_list : state list;
                transition_map : instruction StateCharMap.t
            }

        let make (initial : state) (state_list : state list) : t =
            {
                initial = initial;
                state_list = initial :: state_list;
                transition_map = StateCharMap.empty
            }

        let initial (machine : t) : state =
            machine.initial

        let add_transition (st : state) (c : char) (st' : state) (c' : char) (dir : direction) (machine : t) : t =
            { machine with transition_map = StateCharMap.add (st, c) (st', c', dir) machine.transition_map }

        let step (machine : t) (st : state) (tape : Tape.t) : (state * Tape.t) option =
            let opt_wrapper (f : ('a -> 'b)) (x_opt : 'a option) : 'b option =
                match x_opt with
                | None -> None
                | Some x -> Some (f x)
            and execute_step ((st', c', dir) : instruction) : state * Tape.t =
                (st', Tape.write c' tape |> Tape.move dir)
            in
            StateCharMap.find_opt (st, Tape.read tape) machine.transition_map
            |> opt_wrapper execute_step
    end

# %%
module Machine : MACHINE_EXTRA =
    struct
        (* Turing Machine implementation using two dimensional array for representing the transition function: *)

        (* -`make` function sets saves the inital values and converts the necessary structures - O(n) *)
        (* -`initial` function just returns the stored initial state - O(1) *)
        (* -`initial_id` function just returns the stored initial state id - O(1) *)
        (* -`add_transition` function adds a mapping to the transition stack and does so in constant time due to hashtable lookup - O(1) *)
        (* -`setup` function prepares the Machine for use of the `step` and `speed_step` functions - O(n) *)
        (* -`step` function returns the resulting next state and tape calculated from the given current state and tape. *)
        (*         operations on the tape are executed in O(1) time and searching the transition matrix takes O(1) - O(1) *)
        (* -`speed_step` function is a faster implementation of the `step` function which accepts and returns state ids directly, *)
        (*         avoiding having to use lookup tables at all *)

        (* By performing a trick where we keep the transition matrix empty until we call the `setup` function, we gain a O(1) `step` function. *)
        (* Even though arrays are mutable, we only copy and empty array each time we need to reutrn a new machine, keeping the old machine intact. *)
        (* This requires calling the `setup` function before being able to use `step`. *)

        (* We can also keep some of the values stored in a mutable structure due to the way Machine is implemented. *)
        (* Storing the states in an array and having a state id lookup table as a hashtable (both mutable types) can not cause problems with changing previous machines. *)
        (* This is due to not being able to change add or remove any states after passing them to the `make` function. *)
        (* Meaning that machines that are returned will always have the same defined states as the original, even if the transition function is different. *)

        (* Lastly defining two new types for storing the id of states (their index in the matrix) and instructions with a valid undefined state allow for easier macthing. *)

        type state_id =
            | EndState of state
            | State of int

        type instruction =
            | Undefined
            | Instruction of state_id * char * direction

        type t =
            {
                initial_state : state;
                initial_id : state_id;

                state_array : state array;
                state_id_lookup : (state, int) Hashtbl.t;
                state_amount : int;

                transition_stack : ((int * int) * instruction) list;
                transition_matrix : instruction array array;

                is_setup : bool
            }

        let make (initial_st : state) (st_list : state list) : t =
            let st_list' : state list = initial_st :: st_list
            in
            let st_list_len : int = List.length st_list'
            in
            let id_lookup : (state, int) Hashtbl.t = Hashtbl.create st_list_len
            in
            List.iteri (fun (i : int) (st : state) -> Hashtbl.add id_lookup st i) st_list';
            {
                initial_state = initial_st;
                initial_id = State (Hashtbl.find id_lookup initial_st);

                state_array = Array.of_list st_list';
                state_id_lookup = id_lookup;
                state_amount = st_list_len;

                transition_stack = [];
                transition_matrix = [||];

                is_setup = false
            }

        let initial (machine : t) : state =
            machine.initial_state

        let initial_id (machine : t) : state_id =
            machine.initial_id

        let add_transition (st : state) (c : char) (st' : state) (c' : char) (dir : direction) (machine : t) : t =
            match Hashtbl.find_opt machine.state_id_lookup st with
            | None -> { machine with transition_matrix = [||]; is_setup = false }
            | Some id ->
                let inst : instruction =
                    match Hashtbl.find_opt machine.state_id_lookup st' with
                    | None -> Instruction (EndState st', c', dir)
                    | Some id' -> Instruction (State id', c', dir)
                in
                {
                    machine with
                    transition_stack = ((id, Char.code c), inst) :: machine.transition_stack;
                    transition_matrix = [||];

                    is_setup = false
                }

        let setup (machine : t) : t =
            let machine' : t =
                {
                    machine with
                    transition_matrix =
                        Array.init machine.state_amount (fun (i : int) : instruction array -> Array.make 256 Undefined);

                    is_setup = true
                }
            in
            let rec setup_aux (stack : ((int * int) * instruction) list) : t =
                match stack with
                | [] -> machine'
                | ((i, j), inst) :: tl ->
                   machine'.transition_matrix.(i).(j) <- inst;
                    setup_aux tl
            in
            setup_aux machine'.transition_stack

        let step (machine : t) (st : state) (tape : Tape.t) : (state * Tape.t) option =
            let get_state (st_id : state_id) : state =
                match st_id with
                | EndState st' -> st'
                | State id -> machine.state_array.(id)
            in
            let execute_step (inst : instruction) : (state * Tape.t) option =
                match inst with
                | Undefined -> None
                | Instruction (st_id', c', dir) -> Some (get_state st_id', Tape.write c' tape |> Tape.move dir)
            in
            match Hashtbl.find_opt machine.state_id_lookup st with
            | None -> None
            | Some id ->
                machine.transition_matrix.(id).(Tape.read tape |> Char.code)
                |> execute_step

        let speed_step (machine : t) (st_id : state_id) (tape : Tape.t) : (state_id * Tape.t) option =
            let execute_step (inst : instruction) : (state_id * Tape.t) option =
                match inst with
                | Undefined -> None
                | Instruction (st_id', c', dir) -> Some (st_id', Tape.write c' tape |> Tape.move dir)
            in
            match st_id with
            | EndState _ -> None
            | State id ->
                machine.transition_matrix.(id).(Tape.read tape |> Char.code)
                |> execute_step
    end

# %% [markdown]
# Primer stroja "Binary Increment" na <http://turingmachine.io> lahko implementiramo kot:

# %%
let binary_increment : Machine.t =
    Machine.(
        make "right" [ "carry"; "done" ]
        |> add_transition "right" '1' "right" '1' Right
        |> add_transition "right" '0' "right" '0' Right
        |> add_transition "right" ' ' "carry" ' ' Left
        |> add_transition "carry" '1' "carry" '0' Left
        |> add_transition "carry" '0' "done" '1' Left
        |> add_transition "carry" ' ' "done" '1' Left
    )

# %% [markdown]
# Zapišite funkciji `slow_run` in `speed_run` tipa `Machine.t -> str -> unit`, ki simulirata Turingov stroj na traku, na katerem je na začetku zapisan dani niz. Prva naj izpiše trakove in stanja pri vseh vmesnih korakih, druga pa naj izpiše le končni trak. Slednjo bomo uporabljali tudi pri meritvi učinkovitosti izvajanja.

# %%
let slow_run (machine : Machine.t) (s : string) : unit =
    let machine' : Machine.t = Machine.setup machine
    in
    let output (st : state) (tape : Tape.t) : unit =
        Tape.print tape;
        print_endline st;
        print_newline ()
    in
    let rec slow_run_aux ((st, tape) : state * Tape.t) : unit =
        match Machine.step machine' st tape with
        | None -> output st tape
        | Some (st', tape') ->
            output st tape;
            slow_run_aux (st', tape')
    in
    slow_run_aux (Machine.initial machine', Tape.make s)

# %%
let speed_run (machine : Machine.t) (s : string) : unit =
    let machine' : Machine.t = Machine.setup machine
    in
    let rec speed_run_aux ((st_id, tape) : Machine.state_id * Tape.t) : unit =
        match Machine.speed_step machine' st_id tape with
        | None -> Tape.print tape
        | Some (st_id', tape') -> speed_run_aux (st_id', tape')
    in
    speed_run_aux (Machine.initial_id machine', Tape.make s)

# %%
let primer_slow_run : unit =
    slow_run binary_increment "1011"

# %%
let primer_speed_run : unit =
    speed_run binary_increment "1011"

# %% [markdown]
# ## Krajši zapis

# %% [markdown]
# Ko definiramo Turingov stroj, prehode običajno združujemo najprej po stanjih, nato pa še po znakih. Prav tako pri dosti prehodih samo premikamo glavo, trak in stanje pa pustimo pri miru. Zapišite funkcije:

# - `for_state`
# - `for_character`
# - `for_characters`
# - `move`
# - `switch_and_move`
# - `write_and_move`
# - `write_switch_and_move`
#
# s katerimi bi lahko zgornji primer na krajše zapisali kot spodaj. Implementacijo in tipe ugotovite sami.

# %%
type state_transform = state -> Machine.t -> Machine.t
type char_state_transform = char -> state_transform

# %%
let for_state (st : state) (trans_list : state_transform list list) (machine : Machine.t) : Machine.t =
        List.flatten trans_list
        |> List.fold_left
            (
                fun (machine : Machine.t) (trans : state_transform) : Machine.t ->
                trans st machine
            )
            machine

let for_character (c : char) (trans : char_state_transform) : state_transform list =
    [trans c]

let for_characters (s : string) (trans : char_state_transform) : state_transform list =
    let str_to_list (s : string) : char list =
        List.init (String.length s) (String.get s)
    in
    str_to_list s
    |> List.map trans

let move (dir : direction) : char_state_transform =
    fun (c : char) (st : state) (machine : Machine.t) : Machine.t ->
        Machine.add_transition st c st c dir machine

let switch_and_move (st' : state) (dir : direction) : char_state_transform =
    fun (c : char) (st : state) (machine : Machine.t) : Machine.t ->
        Machine.add_transition st c st' c dir machine

let write_and_move (c' : char) (dir : direction) : char_state_transform =
    fun (c : char) (st : state) (machine : Machine.t) : Machine.t ->
        Machine.add_transition st c st c' dir machine

let write_switch_and_move (c' : char) (st' : state) (dir : direction) : char_state_transform =
    fun (c : char) (st : state) (machine : Machine.t) : Machine.t ->
        Machine.add_transition st c st' c' dir machine

# %%
let binary_increment' : Machine.t =
    Machine.make "right" ["carry"; "done"]
    |> for_state "right"
        [
            for_characters "01" @@ move Right;
            for_character ' ' @@ switch_and_move "carry" Left
        ]
    |> for_state "carry"
        [
            for_character '1' @@ write_switch_and_move '0' "carry" Left;
            for_characters "0 " @@ write_switch_and_move '1' "done" Left
        ]

# %%
let primer_krajse_dodatno_1 : unit =
    slow_run binary_increment' "1011"

# %%
let primer_krajse_dodatno_2 : unit =
    speed_run binary_increment' "1011"

# %% [markdown]
# ## Primeri Turingovih strojev

# %% [markdown]
# Pri tej nalogi boste sestavljali stroje, ki bodo iz začetnega niza na traku na različne načine izračunali nov niz. Pri tem lahko predpostavite, da je začetni niz sestavljen iz ničel in enic, preostanek traku pa je prazen. Na koncu izvajanja naj bo glava na začetku novega niza, z izjemo tega niza pa naj bo trak prazen. Ni pa treba, da se izračunani niz začne na istem mestu na traku, kot se je začel prvotni niz.

# %% [markdown]
# ### Obračanje niza
#

# %% [markdown]
# Sestavite Turingov stroj, ki začetni niz obrne na glavo.

# %%
let reverse : Machine.t =
    Machine.make "rename" ["copy"; "write_zero"; "write_one"; "clear"]
    |> for_state "rename"
        [
            for_character '0' @@ write_and_move 'A' Right;
            for_character '1' @@ write_and_move 'B' Right;
            for_character ' ' @@ switch_and_move "copy" Left
        ]
    |> for_state "copy"
        [
            for_characters "_01" @@ move Left;
            for_character 'A' @@ write_switch_and_move '_' "write_zero" Right;
            for_character 'B' @@ write_switch_and_move '_' "write_one" Right;
            for_character ' ' @@ switch_and_move "clear" Right
        ]
    |> for_state "write_zero"
        [
            for_characters "_01" @@ move Right;
            for_character ' ' @@ write_switch_and_move '0' "copy" Left
        ]
    |> for_state "write_one"
        [
            for_characters "_01" @@ move Right;
            for_character ' ' @@ write_switch_and_move '1' "copy" Left
        ]
    |> for_state "clear"
        [
            for_character '_' @@ write_and_move ' ' Right
        ]

# %%
let primer_reverse : unit =
    speed_run reverse "0000111001"

# %%
let primer_reverse_dodatno : unit =
    speed_run reverse ""

# %% [markdown]
# ### Podvajanje niza

# %% [markdown]
# Sestavite Turingov stroj, ki podvoji začetni niz.

# %%
let duplicate : Machine.t =
    Machine.make "rename" ["duplicate"; "write_zero"; "write_one"; "shift_A"; "shift_B"; "return"]
    |> for_state "rename"
        [
            for_character '0' @@ write_and_move 'A' Right;
            for_character '1' @@ write_and_move 'B' Right;
            for_character ' ' @@ switch_and_move "duplicate" Left
        ]
    |> for_state "duplicate"
        [
            for_character 'A' @@ write_switch_and_move '0' "write_zero" Left;
            for_character 'B' @@ write_switch_and_move '1' "write_one" Left;
            for_characters "01" @@ move Left;
            for_character ' ' @@ switch_and_move "end" Right
        ]
    |> for_state "write_zero"
        [
            for_character 'A' @@ write_switch_and_move '0' "shift_A" Left;
            for_character 'B' @@ write_switch_and_move '0' "shift_B" Left;
            for_character ' ' @@ write_switch_and_move '0' "return" Right
        ]
    |> for_state "write_one"
        [
            for_character 'A' @@ write_switch_and_move '1' "shift_A" Left;
            for_character 'B' @@ write_switch_and_move '1' "shift_B" Left;
            for_character ' ' @@ write_switch_and_move '1' "return" Right
        ]
    |> for_state "shift_A"
        [
            for_character 'A' @@ write_and_move 'A' Left;
            for_character 'B' @@ write_switch_and_move 'A' "shift_B" Left;
            for_character ' ' @@ write_switch_and_move 'A' "return" Right
        ]
    |> for_state "shift_B"
        [
            for_character 'A' @@ write_switch_and_move 'B' "shift_A" Left;
            for_character 'B' @@ write_and_move 'B' Left;
            for_character ' ' @@ write_switch_and_move 'B' "return" Right
        ]
    |> for_state "return"
        [
            for_characters "AB" @@ move Right;
            for_characters "01" @@ switch_and_move "duplicate" Left
        ]

# %%
let primer_duplicate : unit =
    speed_run duplicate "010011"

# %%
let primer_duplicate_dodatno_1 : unit =
    speed_run duplicate "1"

# %%
let primer_duplicate_dodatno_2 : unit =
    speed_run duplicate ""

# %% [markdown]
# ### Eniški zapis
#

# %% [markdown]
# Sestavite Turingov stroj, ki na začetku na traku sprejme število $n$, zapisano v dvojiškem zapisu, na koncu pa naj bo na traku zapisanih natanko $n$ enic.

# %%
let to_unary : Machine.t =
    Machine.make "rename" ["subtract"; "add"; "clear"]
    |> for_state "rename"
        [
            for_character '0' @@ write_and_move 'A' Right;
            for_character '1' @@ write_and_move 'B' Right;
            for_character ' ' @@ switch_and_move "subtract" Left
        ]
    |> for_state "subtract"
        [
            for_character 'A' @@ write_and_move 'B' Left;
            for_character 'B' @@ write_switch_and_move 'A' "add" Right;
            for_character '1' @@ move Left;
            for_character ' ' @@ switch_and_move "clear" Right
        ]
    |> for_state "add"
        [
            for_characters "1AB" @@ move Right;
            for_character ' ' @@ write_switch_and_move '1' "subtract" Left
        ]
    |> for_state "clear"
        [
            for_character 'B' @@ write_and_move ' ' Right
        ]

# %%
let primer_to_unary : unit =
    speed_run to_unary "1010"

# %%
let primer_to_unary_dodatno : unit =
    speed_run to_unary "0"

# %% [markdown]
# ### Dvojiški zapis
#

# %% [markdown]
# Sestavite ravno obratni Turingov stroj, torej tak, ki na začetku na traku sprejme število $n$ enic, na koncu pa naj bo na traku zapisano število $n$ v dvojiškem zapisu.

# %%
let to_binary : Machine.t =
    Machine.make "rename" ["return"; "subtract"; "add"; "clear"]
    |> for_state "rename"
        [
            for_character '1' @@ write_and_move 'A' Right;
            for_character ' ' @@ switch_and_move "return" Left
        ]
    |> for_state "return"
        [
            for_character 'A' @@ move Left;
            for_character ' ' @@ switch_and_move "subtract" Right;
        ]
    |> for_state "subtract"
        [
            for_character 'A' @@ write_switch_and_move '_' "add" Left;
            for_characters "_01" @@ move Right;
            for_character ' ' @@ switch_and_move "clear" Left
        ]
    |> for_state "add"
        [
            for_character '_' @@ move Left;
            for_characters " 0" @@ write_switch_and_move '1' "subtract" Right;
            for_character '1' @@ write_and_move '0' Left
        ]
    |> for_state "clear"
        [
            for_character '_' @@ write_and_move ' ' Left;
            for_characters "01" @@ move Left;
            for_character ' ' @@ switch_and_move "end" Right;
        ]

# %%
let primer_to_binary : unit =
    speed_run to_binary (String.make 42 '1')

# %%
let primer_to_binary_dodatno : unit =
    speed_run to_binary "1"

# %%
let time (f : unit -> 'a) : 'a =
    let start_time : float = Sys.time()
    in
    let res : 'a = f ()
    in
    Printf.printf "Execution time: %fs" (Sys.time() -. start_time);
    print_newline ();
    res

# %%
let busy_beaver5 : Machine.t =
    Machine.(
        make "A" ["B"; "C"; "D"; "E"]
        |> add_transition "A" ' ' "B" '1' Right
        |> add_transition "A" '1' "C" '1' Left
        |> add_transition "B" ' ' "C" '1' Right
        |> add_transition "B" '1' "B" '1' Right
        |> add_transition "C" ' ' "D" '1' Right
        |> add_transition "C" '1' "E" ' ' Left
        |> add_transition "D" ' ' "A" '1' Left
        |> add_transition "D" '1' "D" '1' Left
        |> add_transition "E" '1' "A" ' ' Left
    )

# %%
let test = time (fun () -> speed_run busy_beaver5 "")
