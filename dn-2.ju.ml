# %% [markdown]
# # 2. domača naloga

# %% [markdown]
# Pri tej nalogi boste napisali svoj simulator računalnika, ki se bo malenkostno razlikoval od [tistega, ki smo ga spoznali na predavanjih](https://schweigi.github.io/assembler-simulator/):
# - Simulator bo uporabljal Harvardsko arhitekturo, kar pomeni, da bo ločil med pomnilnikoma za program in podatke.
# - Namesto pomnilnika z omejeno velikostjo bomo imeli samo sklad, ki ga bomo predstavili s poljubno velikim seznamom.
# - Prav tako ne bomo vsega predstavili z 8-bitnimi števili. Za ukaze bomo definirali svoj naštevni tip, števila v pomnilniku pa bodo taka, kot jih podpira OCaml.

# %% [markdown]
# ## Podatkovni tipi

# %% [markdown]
# Pri vsakem večjem funkcijskem programu je prvi korak definicija ustreznih tipov. V simulatorju bomo imeli dva glavna tipa: `instruction`, s katerim bomo predstavili posamezne ukaze v programu, in `state`, s katerim bomo predstavili trenutno stanje računalnika. Seveda pa si bomo morali pred njima definirati še nekaj pomožnih tipov.

# %% [markdown]
# ### Registri

# %% [markdown]
# Nekateri ukazi za argument sprejmejo register, ki ga spreminjajo, na primer: `INC A` ali `POP B`.

# Definirajte naštevni tip `register`, ki bo predstavljal štiri možne registre procesorja **A**, **B**, **C** in **D**.

# %%
type register =
	| A
	| B
	| C
	| D

# %%
let primer_tipi_1 : register list list = [[A; B; B; A]; [A; C; D; C]]

# %% [markdown]
# ### Izrazi

# %% [markdown]
# Nekateri ukazi poleg registra sprejmejo še dodaten argument, ki je lahko bodisi register, bodisi celoštevilska konstanta, na primer `MOV A, B` ali `MOV A, 42`. Definirajte naštevni tip `expression`, ki predstavlja izraze, ki so lahko registri ali števila.

# %%
type expression =
	| Register of register
	| Const of int

# %%
let primer_tipi_2 : expression list = [Register B; Const 42]

# %% [markdown]
# ### Naslovi

# %% [markdown]
# Ukazi za skoke za argument sprejmejo naslov ukaza v pomnilniku. Naslove bomo predstavili s celimi števili, da pa jih ne bi ponesreči zamešali s celoštevilskimi konstantami, definirajte še tip `address`, ki naj bo naštevni tip z eno samo varianto `Address` s celoštevilskim argumentom.

# %%
type address = Address of int

# %%
let primer_tipi_3 : int * address = (42, Address 42)

# %% [markdown]
# ### Ukazi

# %% [markdown]
# Naš simulator bo podpiral naslednje ukaze, pri čemer je _R_ vedno poljuben register, _A_ naslov v ukaznem pomnilniku, _E_ pa izraz, torej bodisi register bodisi celoštevilska konstanta.

# ukaz						| opis
# ------------------------: | -----------------------------------------------------------------------------------------------------
# `MOV` _R_, _E_			| premakni vrednost izraza _E_ v register _R_
# `ADD`/`SUB` _R_, _E_		| register _R_ povečaj/zmanjšaj za _E_
# `INC`/`DEC` _R_			| register _R_ povečaj/zmanjšaj za 1
# `MUL`/`DIV` _E_			| register **A** pomnoži/deli z _E_
# `AND`/`OR`/`XOR` _R_, _E_ | v register _R_ shrani rezultat logične operacije _R op E_
# `NOT` _R_					| negiraj register _R_
# `CMP` _R_, _E_			| primerjaj register _R_ z vrednostjo _E_ ter rezultat primerjave shrani v zastavici **Zero** in **Carry**
# `JMP` _A_					| skoči na naslov _A_
# `JA`/`JAE` _A_			| skoči na naslov _A_, če je v zadnji primerjavi veljalo _x > y_ / _x ≥ y_
# `JB`/`JBE` _A_			| skoči na naslov _A_, če je v zadnji primerjavi veljalo _x < y_ / _x ≤ y_
# `JE`/`JNE` _A_			| skoči na naslov _A_, če je v zadnji primerjavi veljalo _x = y_ / _x ≠ y_
# `CALL` _A_				| skoči na naslov _A_ in shrani naslov naslednjega ukaza na vrh sklada
# `RET`						| iz funkcije se vrni na naslov na vrhu sklada
# `PUSH` _E_				| vrednost izraza _E_ shrani na vrh sklada
# `POP` _R_					| snemi vrednost s sklada in jo shrani v register _R_
# `HLT`						| ustavi izvajanje programa

# %% [markdown]
# Dopolnite naslednjo definicijo tipa `instruction`, da bo imel eno varianto za vsakega od zgoraj navedenih ukazov:

# %%
type instruction =
	| MOV of register * expression
	| ADD of register * expression
	| SUB of register * expression
	| INC of register
	| DEC of register
	| MUL of expression
	| DIV of expression
	| AND of register * expression
	| OR of register * expression
	| XOR of register * expression
	| NOT of register
	| CMP of register * expression
	| JMP of address
	| JA of address
	| JAE of address
	| JB of address
	| JBE of address
	| JE of address
	| JNE of address
	| CALL of address
	| RET
	| PUSH of expression
	| POP of register
	| HLT

# %%
let primer_tipi_4 : instruction list = [ MOV (A, Register B); MOV (C, Const 42); JA (Address 10); HLT ]

# %% [markdown]
# Za primer večjega programa se spomnimo programa za izračun Fibonaccijevih števil. S seznamom ukazov, bi ga napisali kot spodaj. Pri tem opazite, da so naslovi ukazov v programu zapisani kot celoštevilski indeksi. Pretvorbo iz berljivih oznak kot so `main`, `fib` in `.fib_end` bomo obravnavali kasneje.

# %%
let fibonacci n =
	[
		JMP (Address 20);		(* JMP main *)

		(* fib: *)
			(* ; Shranimo vrednosti registrov *)
			PUSH (Register C);		(* PUSH C *)
			PUSH (Register B);		(* PUSH B *)

			(* ; V C shranimo začetno vrednost A *)
			MOV (C, Register A);	(* MOV C, A *)

			(* ; Če je A = 0, je to tudi rezultat *)
			CMP (A, Const 0);		(* CMP A, 0 *)
			JE (Address 17);		(* JE .fib_end *)

			(* ; Če je A = 1, je to tudi rezultat *)
			CMP (A, Const 1);		(* CMP A, 1 *)
			JE (Address 17);		(* JE .fib_end *)

			(* ; V nasprotnem primeru najprej izračunamo fib(A - 1) in ga shranimo v B *)
			DEC C;					(* DEC C *)
			MOV (A, Register C);	(* MOV A, C *)
			CALL (Address 1);		(* CALL fib *)
			MOV (B, Register A);	(* MOV B, A *)

			(* ; Nato izračunamo še fib(A - 2) in ga shranimo v A *)
			DEC C;					(* DEC C *)
			MOV (A, Register C);	(* MOV A, C *)
			CALL (Address 1);		(* CALL fib *)

			(* ; Nazadnje k A prištejemo še B, s čimer dobimo končni rezultat *)
			ADD (A, Register B);	(* ADD A, B *)
			JMP (Address 17);		(* JMP .fib_end *)

		(* .fib_end: *)
			(* ; Povrnemo vrednosti registrov in vrnemo rezultat *)
			POP B;					(* POP B *)
			POP C;					(* POP C *)
			RET;					(* RET *)

		(* main: *)
			MOV (A, Const n);		(* MOV A, n *)
			CALL (Address 1);		(* CALL fib *)
			HLT;					(* HLT *)
	]

# %%
let primer_tipi_5 : instruction list = fibonacci 10

# %% [markdown]
# ### Pomnilnik

# %% [markdown]
# Morda v nasprotju s pričakovanji ukazov ne bomo shranjevali v sezname tipa `instruction list`, ampak v tabele tipa `instruction array`. O tabelah se bomo še pogovarjali, njihova bistvena prednost pa je ta, da do elementa na danem mestu lahko dostopamo takoj, ne da bi se morali sprehoditi po predhodnih elementih. Tabele pišemo tako kot sezname, le da oklepaje pišemo kot `[| ... |]` namesto kot `[ ... ]`, do posameznega elementa tabele pa dostopamo prek `tabela.(indeks)`, na primer `[| 314; 42; 2718 |].(1)` vrne `42`.

# %% [markdown]
# Nazadnje bomo celotno stanje računalnika predstavili z zapisnim. Definirajte tip `state` s sledečimi polji:
# - `instructions`: tabela ukazov v ukaznem pomnilniku,
# - `a`, `b`, `c`, `d`: štiri celoštevilske vrednosti v registrih,
# - `ip`: naslov trenutnega ukaza, tipa `address`,
# - `zero`, `carry`: vrednosti zastavic **Zero** in **Carry**,
# - `stack`: seznam celoštevilskih vrednosti na skladu.

# %%
type state =
	{
		instructions : instruction array;
		a : int;
		b : int;
		c : int;
		d : int;
		ip : address;
		zero : bool;
		carry : bool;
		stack : int list;
	}

# %%
let primer_tipi_6 : state =
	{
		instructions = [| MOV (A, Register B); MOV (C, Const 42); JA (Address 10); HLT |];
		a = 1; b = 2; c = 3; d = 4;
		ip = Address 0;
		zero = true; carry = false;
		stack = [5; 6; 7];
	}

# %% [markdown]
# ### Začetno stanje

# %% [markdown]
# Prazno stanje pomnilnika lahko predstavimo z zapisom:

# %%
let empty : state =
	{
		instructions = [||];
		a = 0;
		b = 0;
		c = 0;
		d = 0;
		ip = Address 0;
		zero = false;
		carry = false;
		stack = [];
	}

# %% [markdown]
# Kljub temu, da so tabele učinkovitejše, so seznami za delo bolj praktični. Zato definirajte funkcijo `init : instruction list -> state`, ki sprejme seznam ukazov in vrne začetno stanje računalnika, v katerem so vsi registri in zastavice nastavljeni na nič, sklad pa je prazen. Pri tem si lahko za pretvorbo seznama v tabelo pomagate z uporabo funkcije `Array.of_list`.

# %%
let init (inst_list : instruction list) : state =
	{ empty with instructions = Array.of_list inst_list }

# %%
let primer_tipi_7 : state = init [ MOV (A, Register B); MOV (C, Const 42); JA (Address 10); HLT ]

# %% [markdown]
# ## Izvajanje ukazov

# %% [markdown]
# S pripravljenima tipoma ukazov in stanja se lahko lotimo pisanja funkcij za izvrševanje ukazov.

# %% [markdown]
# ### Branje stanja

# %% [markdown]
# Napišite funkcijo `read_instruction : state -> instruction option`, ki v danem stanju vrne trenuten ukaz. Če ukaz sega izven območja ukaznega pomnilnika, naj funkcija vrne `None`.

# %%
let read_instruction (state : state) : instruction option =
	let (Address i : address) = state.ip
	in
	if i < 0 || Array.length state.instructions <= i then None
	else Some state.instructions.(i)

# %%
let primer_izvajanje_1 : instruction option list =
	[
		read_instruction { empty with instructions = [| MOV (A, Register B); MOV (C, Const 42); JA (Address 10); HLT |]; ip = (Address 1) };
		read_instruction { empty with instructions = [| MOV (A, Register B); MOV (C, Const 42); JA (Address 10); HLT |]; ip = (Address 3) };
		read_instruction { empty with instructions = [| MOV (A, Register B); MOV (C, Const 42); JA (Address 10); HLT |]; ip = (Address 5) };
	]

# %% [markdown]
# Napišite funkcijo `read_register : state -> register -> int`, ki vrne vrednost registra v danem stanju.

# %%
let read_register (state : state) (reg : register) : int =
	match reg with
	| A -> state.a
	| B -> state.b
	| C -> state.c
	| D -> state.d

# %%
let primer_izvajanje_2 : int = read_register { empty with a = 10; b = 42 } B

# %% [markdown]
# Napišite funkcijo `read_expression : state -> expression -> int`, ki vrne celoštevilsko vrednost izraza v danem stanju.

# %%
let read_expression (state : state) (expr : expression) : int =
	match expr with
	| Register reg -> read_register state reg
	| Const c -> c

# %%
let primer_izvajanje_3 : int = read_expression { empty with a = 10; b = 20 } (Register B)

# %%
let primer_izvajanje_4 : int = read_expression { empty with a = 10; b = 20 } (Const 42)

# %% [markdown]
# ### Spreminjanje registrov

# %% [markdown]
# Napišite funkcijo `write_register : state -> register -> int -> state`, ki vrednost registra v danem stanju nastavi na dano število. Funkcija naj vrne novo stanje.

# %%
let write_register (state : state) (reg : register) (n : int) : state =
	match reg with
	| A -> { state with a = n }
	| B -> { state with b = n }
	| C -> { state with c = n }
	| D -> { state with d = n }

# %%
let primer_izvajanje_5 : state = write_register { empty with c = 42 } D 24

# %% [markdown]
# Napišite funkcijo `perform_unop : (int -> int) -> state -> register -> state`, ki izvede eniško operacijo na vrednosti registra. Funkcija naj vrne novo stanje s spremenjenim registrom.

# %%
let perform_unop (unop : int -> int) (state : state) (reg : register) : state =
	read_register state reg
	|> unop
	|> write_register state reg

# %%
let primer_izvajanje_6 : state = perform_unop (fun x -> 101 * x) { empty with c = 5 } C

# %% [markdown]
# Napišite funkcijo `perform_binop : (int -> int -> int) -> state -> register -> expression -> state`, ki izvede dvojiško operacijo na danem registru in izrazu. Funkcija naj vrne novo stanje s spremenjenim registrom.

# %%
let perform_binop (binop : int -> int -> int) (state : state) (reg : register) (expr : expression) : state =
	read_expression state expr
	|> binop (read_register state reg)
	|> write_register state reg

# %%
let primer_izvajanje_7 : state = perform_binop ( * ) { empty with c = 5 } C (Const 101)

# %% [markdown]
# ### Skoki

# %% [markdown]
# Napišite funkcijo `next : address -> address`, ki vrne naslednji naslov (torej povečan za 1, saj v našem primeru vsi ukazi zasedejo enako prostora).

# %%
let next (Address i : address) : address = Address (i + 1)

# %%
let primer_izvajanje_8 : address = next (Address 41)

# %% [markdown]
# Napišite funkciji `jump : state -> address -> state` in `proceed : state -> state`. Prva naj v danem stanju skoči na dani naslov, druga pa naj skoči na naslednji ukaz.

# %%
let jump (state : state) (addr : address) : state =
	{ state with ip = addr }

let proceed (state : state) : state =
	{ state with ip = next state.ip }

# %%
let primer_izvajanje_8 : state = jump { empty with ip = Address 42} (Address 10)

# %%
let primer_izvajanje_10 : state = proceed { empty with ip = Address 42}

# %% [markdown]
# Napišite funkciji `push_stack : state -> int -> state` in `pop_stack : state -> int * state`, ki dodata vrednost na sklad oziroma jo odstranita z njega. Funkcija `pop_stack` poleg spremenjenega stanja vrne tudi odstranjeno vrednost. Če je sklad prazen, naj funkcija `pop_stack` sproži izjemo.

# %%
let push_stack (state : state) (n : int) : state =
	{ state with stack = n :: state.stack }

let pop_stack (state : state) : int * state =
	match state.stack with
	| [] -> failwith "Cannot pop from empty stack"
	| hd :: tl -> (hd, { state with stack = tl })

# %%
let primer_izvajanje_10 : state = push_stack { empty with stack = [1; 2; 3] } 42

# %%
let primer_izvajanje_11 : int * state = pop_stack { empty with stack = [1; 2; 3] }

# %% [markdown]
# ### Pogojni skoki

# %% [markdown]
# Napišite funkcijo `compare : state -> int -> int -> state`, ki primerja vrednosti dveh števil in ustrezno nastavi zastavici **Zero** in **Carry**. Prvo naj nastavi na `true` natanko tedaj, kadar sta števili enaki, drugo pa takrat, kadar je prvo število manjše.Funkcija naj vrne novo stanje.

# %%
let compare (state : state) (n : int) (m : int) : state =
	{ state with zero = (n = m); carry = (n < m) }

# %%
let primer_izvajanje_12 : state = compare empty 24 42

# %%
let primer_izvajanje_dodatno_1 : state = compare empty 24 24

# %%
let primer_izvajanje_dodatno_2 : state = compare empty 42 24

# %% [markdown]
# Napišite funkcijo `conditional_jump : state -> address -> bool -> state`, ki skoči na dani naslov, če je podan pogoj izpolnjen. V nasprotnem primeru naj funkcija skoči na naslednji ukaz.

# %%
let conditional_jump (state : state) (addr : address) (phi : bool) : state =
	if phi then jump state addr
	else proceed state

# %%
let primer_izvajanje_13 : state = conditional_jump { empty with ip = Address 42 } (Address 10) true

# %%
let primer_izvajanje_14 : state = conditional_jump { empty with ip = Address 42 } (Address 10) false

# %% [markdown]
# ### Klici funkcij

# %% [markdown]
# Napišite funkcijo `call : state -> address -> state`, ki v danem stanju skoči na dani naslov in na sklad doda naslednji naslov.

# %%
let call (state : state) (addr : address) : state =
	let (Address i : address) = next state.ip
	in
	jump (push_stack state i) addr

# %%
let primer_izvajanje_15 : state = call { empty with ip = Address 42 } (Address 10)

# %% [markdown]
# Napišite funkcijo `return : state -> state`, ki v danem stanju skoči na naslov, ki je na vrhu sklada, in odstrani ta naslov s sklada.

# %%
let return (state : state) : state =
	let (i, state') : int * state = pop_stack state
	in
	{ state' with ip = Address i }

# %%
let primer_izvajanje_16 : state = return { empty with ip = (Address 100); stack = [42; 43; 44] }

# %% [markdown]
# ### Izvajanje programov

# %% [markdown]
# S pomočjo zgoraj definiranih funkcij dopolnite funkcijo `run_instruction : state -> instruction -> state`, ki izvede podani ukaz v danem stanju in vrne novo stanje. Za interpretacije pogojnih skokov si lahko pomagate z [navodili simulatorja](https://schweigi.github.io/assembler-simulator/instruction-set.html), ki smo ga pogledali na predavanjih.

# %%
let run_instruction (state : state) (inst : instruction) : state =
	match inst with
	| MOV (reg, expr) -> read_expression state expr |> write_register state reg |> proceed
	| ADD (reg, expr) -> perform_binop ( + ) state reg expr |> proceed
	| SUB (reg, expr) -> perform_binop ( - ) state reg expr |> proceed
	| INC reg -> perform_unop succ state reg |> proceed
	| DEC reg -> perform_unop pred state reg |> proceed
	| MUL expr -> perform_binop ( * ) state A expr |> proceed
	| DIV expr -> perform_binop ( / ) state A expr |> proceed
	(* Pozor, OCaml land/lor/lxor interpretira kot simbole, zato jih pišemo infiksno! *)
	| AND (reg, expr) -> perform_binop ( land ) state reg expr |> proceed
	| OR (reg, expr) -> perform_binop ( lor ) state reg expr |> proceed
	| XOR (reg, expr) -> perform_binop ( lxor ) state reg expr |> proceed
	| NOT reg -> perform_unop lnot state reg |> proceed
	| CMP (reg, expr) -> compare state (read_register state reg) (read_expression state expr) |> proceed
	| JMP addr -> jump state addr
	| JA addr -> conditional_jump state addr (not state.carry && not state.zero)
	| JAE addr -> conditional_jump state addr (not state.carry)
	| JB addr -> conditional_jump state addr (state.carry && not state.zero)
	| JBE addr -> conditional_jump state addr (state.carry)
	| JE addr -> conditional_jump state addr (state.zero)
	| JNE addr -> conditional_jump state addr (not state.zero)
	| CALL addr -> call state addr
	| RET -> return state
	| PUSH expr -> push_stack state (read_expression state expr) |> proceed
	| POP reg ->
		let n, state' = pop_stack state
		in
		write_register state' reg n |> proceed
	| HLT -> failwith "Cannot execute instruction"

# %% [markdown]
# Napišite funkcijo `run_program : state -> state`, ki izvaja ukaze v danem stanju, dokler ne naleti na ukaz `HLT` ali pa ukazni kazalec skoči ven iz ukaznega pomnilnika. Funkcija naj vrne končno stanje.

# %%
let rec run_program (state : state) : state =
	match read_instruction state with
	| (None | Some HLT) -> state
	| Some inst -> run_instruction state inst |> run_program

# %%
let primer_izvajanje_16 : state =
	fibonacci 10
	|> init
	|> run_program

# %% [markdown]
# ## Branje zbirnika

# %% [markdown]
# Da bomo programe lahko pisali v zbirniku, napišimo še funkcije za branje nizov. Predpostavljate lahko, da bodo vsi nizi pravilno oblikovani, zato v primeru napake s `failwith ...` javite ustrezno sporočilo o napaki.

# %% [markdown]
# ### Registri in izrazi

# %% [markdown]
# Napišite funkcijo `parse_register : string -> register`, ki iz niza prebere register.

# %%
let parse_register (reg_str : string) : register =
	match reg_str with
	| "A" -> A
	| "B" -> B
	| "C" -> C
	| "D" -> D
	| _ -> failwith "Cannot convert to register"

# %%
let primer_branje_1 : register = parse_register "A"

# %% [markdown]
# Napišite funkcijo `parse_expression : string -> expression`, ki iz niza prebere izraz.

# %%
let parse_expression (expr_str : string) : expression =
	match int_of_string_opt expr_str with
	| None -> Register (parse_register expr_str)
	| Some n -> Const n

# %%
let primer_branje_2 : expression = parse_expression "A"

# %%
let primer_branje_3 : expression = parse_expression "42"

# %% [markdown]
# ### Čiščenje vrstic

# %% [markdown]
# Napišite funkcijo `clean_line : string -> string`, ki iz niza odstrani vse presledke in komentarje (ki se začnejo z znakom `;`). Pri iskanju in odstranjevanju komentarjev si pomagajte z uporabo funkcij `String.index_opt` in `String.sub`.

# %%
let clean_line (line : string) : string =
	(
		match String.index_opt line ';' with
		| None -> line
		| Some i -> String.sub line 0 i
	)
	|> String.trim

# %%
let primer_branje_4 : string = clean_line "   MOV A, 42    ; To je komentar   "

# %% [markdown]
# Napišite funkcijo `clean_lines : string list -> string list`, ki iz seznama nizov najprej odstrani vse komentarje in presledke, nato pa odstrani vse prazne vrstice.

# %%
let clean_lines (lines : string list) : string list =
	List.map clean_line lines
	|> List.filter (fun (s : string) : bool -> s <> "")

# %%
let primer_branje_dodatno_1 : string list =
	clean_lines
		[
			"	MOV A, 42	 ; To je komentar	";
			"	; Vidim korenje   ";
			"	; Voham korenje   ";
			"	; ČUUUUUUUUUTIM KORENJE   ";
			"	ADD B, 77	 ; To ni komentar	";
		]

# %% [markdown]
# ### Oznake

# %% [markdown]
# Kot smo navajeni iz zbirnika, skokov ne podajamo z indeksi, ampak raje v dele kode napišemo oznake kot so `main:` ali `.loop:`, nato pa se nanje sklicujemo kot `JA .loop`, `JMP main`, `CALL fib` in tako naprej. Oznake bomo hranili v seznamu, ki bo vsaki oznaki priredil ustrezen naslov v ukaznem pomnilniku.

# %% [markdown]
# Napišite funkcijo `parse_address : (string * address) list -> string -> address`, ki pri danem seznamu oznak iz niza prebere naslov. Naslov je lahko podan direktno s številom ali pa z eno izmed oznak v seznamu.

# %%
let parse_address (dict : (string * address) list) (label : string) : address =
	match int_of_string_opt label with
	| Some i -> Address i
	| None ->
		match List.assoc_opt label dict with
		| None -> failwith "Label not found in dictionary"
		| Some addr -> addr

# %%
let primer_branje_5 : address = parse_address [("main", Address 42)] "main"

# %%
let primer_branje_6 : address = parse_address [("main", Address 42)] "123"

# %% [markdown]
# Napišite funkcijo `parse_label : string -> string option`, ki vrne oznako, če se niz konča z dvopičjem, sicer pa vrne `None`.

# %%
let parse_label (label_str : string) : string option =
	if label_str = "" then None
	else
		let len : int = String.length label_str
		in
		if label_str.[len - 1] <> ':' then None
		else Some (String.sub label_str 0 (len - 1))

# %%
let primer_branje_7 : string option = parse_label "main:"

# %%
let primer_branje_8 : string option = parse_label "MOV A, 42"

# %% [markdown]
# Da bomo iz kode določili oznake, napišite funkcijo `parse_labels : string list -> (string * address) list * string list`, ki iz seznama nizov, ki so bodisi oznake bodisi ukazi, izloči oznake in jim priredi naslove, ostale vrstice pa pusti nespremenjene.

# %%
let parse_labels (lines : string list) : (string * address) list * string list =
	let i : int ref = ref 0
	in
	List.partition_map
		(
			fun (line : string) : (string * address, string) Either.t ->
				match parse_label line with
				| None -> i := !i + 1; Right line
				| Some label -> Left (label, Address !i)
		)
		lines

# %%
let primer_branje_9 : (string * address) list * string list = parse_labels ["JMP main"; "main:"; "MOV A, 0"; "loop:"; "INC A"; "JMP loop"; "testest"; ""; ""; ""; "test:"; "BANANA"]

# %% [markdown]
# Dopolnite spodnjo funkcijo `parse_instruction : (string * address) list -> string -> instruction`, ki iz niza prebere ukaz.

# %%
let parse_instruction (labels : (string * address) list) (line : string) : instruction =
	let tokens =
		line
		|> String.split_on_char ' '
		|> List.concat_map (String.split_on_char ',')
		|> List.map String.trim
		|> List.filter (fun token -> token <> "")
	in
	match tokens with
	| ["MOV"; reg; expr] -> MOV (parse_register reg, parse_expression expr)
	| ["ADD"; reg; expr] -> ADD (parse_register reg, parse_expression expr)
	| ["SUB"; reg; expr] -> SUB (parse_register reg, parse_expression expr)
	| ["INC"; reg] -> INC (parse_register reg)
	| ["DEC"; reg] -> DEC (parse_register reg)
	| ["MUL"; expr] -> MUL (parse_expression expr)
	| ["DIV"; expr] -> DIV (parse_expression expr)
	| ["AND"; reg; expr] -> AND (parse_register reg, parse_expression expr)
	| ["OR"; reg; expr] -> OR (parse_register reg, parse_expression expr)
	| ["XOR"; reg; expr] -> XOR (parse_register reg, parse_expression expr)
	| ["NOT"; reg] -> NOT (parse_register reg)
	| ["CMP"; reg; expr] -> CMP (parse_register reg, parse_expression expr)
	| ["JMP"; addr] -> JMP (parse_address labels addr)
	| ["JA"; addr] -> JA (parse_address labels addr)
	| ["JAE"; addr] -> JAE (parse_address labels addr)
	| ["JB"; addr] -> JB (parse_address labels addr)
	| ["JBE"; addr] -> JBE (parse_address labels addr)
	| ["JE"; addr] -> JE (parse_address labels addr)
	| ["JNE"; addr] -> JNE (parse_address labels addr)
	| ["CALL"; addr] -> CALL (parse_address labels addr)
	| ["RET"] -> RET
	| ["PUSH"; expr] -> PUSH (parse_expression expr)
	| ["POP"; reg] -> POP (parse_register reg)
	| ["HLT"] -> HLT
	| _ -> failwith ("Invalid instruction: " ^ line)

# %%
let primer_branje_10 : instruction list = List.map (parse_instruction [("main", Address 42)]) ["MOV A, 42"; "CALL main"; "HLT"]

# %% [markdown]
# S pomočjo zgoraj napisanih funkcij sestavite funkcijo `run : string -> state`, ki niz razbije na vrstice, prebere ukaze in oznake ter pripravi začetno stanje, nato pa program izvaja vse dokler ne naleti na ukaz `HLT`. Po klicu naj funkcija vrne končno stanje.

# %%
let run (code : string) : state =
	code
	|> String.split_on_char '\n'
	|> clean_lines
	|> parse_labels
	|>
		(
			fun ((labels, inst_list) : (string * address) list * string list) : instruction list ->
				List.map (parse_instruction labels) inst_list
		)
	|> init
	|> run_program

# %%
let fibonacci : string =
	{|
		JMP main
		; Funkcija, ki izračuna fib(A) in vrednost shrani v register A
		fib:
			; Shranimo vrednosti registrov
			PUSH C
			PUSH B

			; V C shranimo začetno vrednost A
			MOV C, A

			; Če je A = 0, je to tudi rezultat
			CMP A, 0
			JE .fib_end

			; Če je A = 1, je to tudi rezultat
			CMP A, 1
			JE .fib_end

			; V nasprotnem primeru najprej izračunamo fib(A - 1) in ga shranimo v B
			DEC C
			MOV A, C
			CALL fib
			MOV B, A

			; Nato izračunamo še fib(A - 2) in ga shranimo v A
			DEC C
			MOV A, C
			CALL fib

			; Nazadnje k A prištejemo še B, s čimer dobimo končni rezultat
			ADD A, B
			JMP .fib_end

		.fib_end:
			; Povrnemo vrednosti registrov in vrnemo rezultat
			POP B
			POP C
			RET

		main:
			MOV A, 7
			CALL fib
		HLT
		|}

# %%
let primer_branje_11 : state = run fibonacci
