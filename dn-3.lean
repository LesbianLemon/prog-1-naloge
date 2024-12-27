set_option autoImplicit false

/------------------------------------------------------------------------------
 ## Naravna števila

 Definirajte funkcijo, ki _rekurzivno_ (torej naivno in ne direktno s formulo,
 ki jo boste morali dokazati) sešteje prvih `n` naravnih števil, ter
 dokažite, da zanjo velja znana enakost (najprej v obliki, ki ne zahteva
 deljenja, nato pa še v običajni obliki).
------------------------------------------------------------------------------/

def vsota_prvih : Nat → Nat :=
    fun (n : Nat) =>
        match n with
        | Nat.zero => 0
        | Nat.succ m => n + vsota_prvih m

-- Preizkus vsote naravnih števil
#eval vsota_prvih 0
#eval vsota_prvih 10

theorem gauss : (n : Nat) → 2 * vsota_prvih n = n * (n + 1) :=
    by
        intro n
        induction n with
        | zero =>
            simp [vsota_prvih]
        | succ m ih =>
            calc
                2 * vsota_prvih (m + 1)
                _ = 2 * ((m + 1) + vsota_prvih m) := by simp [vsota_prvih]
                _ = 2 * (m + 1) + 2 * vsota_prvih m := Nat.left_distrib 2 (m + 1) (vsota_prvih m)
                _ = 2 * (m + 1) + m * (m + 1) := by rw [ih]
                _ = (2 + m) * (m + 1) := by rw [Nat.right_distrib]
                _ = (m + 2) * (m + 1) := by rw [Nat.add_comm]
                _ = (m + 1) * (m + 2) := Nat.mul_comm (m + 2) (m + 1)
                _ = (m + 1) * ((m + 1) + 1) := by rw [Nat.add_one]

theorem cisto_pravi_gauss : (n : Nat) → vsota_prvih n = (n * (n + 1)) / 2 :=
    by
        intro n
        have h : n * (n + 1) = 2 * vsota_prvih n := Eq.symm (gauss n)
        apply Eq.symm
        exact Nat.div_eq_of_eq_mul_right Nat.zero_lt_two h


/------------------------------------------------------------------------------
 ## Vektorji

 Definirajmo vektorje podobno kot na predavanjih, le da namesto svojih naravnih
 števil uporabimo vgrajena. Da se tipi ujamejo, funkcijo stikanja napišemo s
 pomočjo taktik.

 Napišite funkcijo `obrni`, ki vrne na glavo obrnjen vektor, ter funkciji
 `glava` in `rep`, ki varno vrneta glavo in rep _nepraznega_ seznama.
------------------------------------------------------------------------------/

inductive Vektor : Type → Nat → Type where
    | prazen : {A : Type} → Vektor A 0
    | sestavljen {A : Type} {n : Nat} : A → Vektor A n → Vektor A (n + 1)
deriving Repr

-- Definiramo dva poskusna vektorja
def empty_nat_vec : Vektor Nat 0 := Vektor.prazen
def test_nat_vec : Vektor Nat 3 := Vektor.sestavljen 1 (Vektor.sestavljen 2 (Vektor.sestavljen 3 Vektor.prazen))

def stakni {A : Type} {m n : Nat} : Vektor A m → Vektor A n → Vektor A (m + n) :=
    fun (xs : Vektor A m) (ys : Vektor A n) =>
        match xs with
        | Vektor.prazen => by rw [Nat.add_comm]; exact ys
        | Vektor.sestavljen x xs' => by rw [Nat.add_right_comm]; exact Vektor.sestavljen x (stakni xs' ys)

def obrni {A : Type} {n : Nat} : Vektor A n → Vektor A n :=
    fun (xs : Vektor A n) =>
        match xs with
        | Vektor.prazen => Vektor.prazen
        | Vektor.sestavljen x xs' => stakni (obrni xs') (Vektor.sestavljen x Vektor.prazen)

-- Funkcija obrni pravilno obrne prazen in sestavljen seznam
#eval obrni empty_nat_vec
#eval obrni test_nat_vec

def glava {A : Type} {n : Nat} : Vektor A (n + 1) → A :=
    fun xs =>
        match xs with
        | Vektor.sestavljen x _ => x

-- Funkcija glava se pravilno pritoži nad praznim seznamom in vrne prvi element sestavljenega
#eval glava empty_nat_vec
#eval glava test_nat_vec

def rep {A : Type} {n : Nat} : Vektor A (n + 1) → Vektor A n :=
    fun xs =>
        match xs with
        | Vektor.sestavljen _ xs' => xs'

-- Funkcija rep se pravilno pritoži nad praznim seznamom in vrne rep sestavljenega
#eval rep empty_nat_vec
#eval rep test_nat_vec

/------------------------------------------------------------------------------
 ## Predikatni račun

 Dokažite spodnje tri trditve. Zadnja je _paradoks pivca_, ki pravi:
     "V vsaki neprazni gostilni obstaja gost, za katerega velja,
     da če pije on, pijejo vsi v gostilni."
 Za dokaz potrebujete klasično logiko, torej nekaj iz modula `Classical`.
------------------------------------------------------------------------------/

theorem forall_implies {A : Type} {P Q : A → Prop} : (∀ x, (P x → Q x)) → (∀ x, P x) → (∀ x, Q x) :=
    by
        intro h1 h2
        intro x
        exact h1 x (h2 x)

theorem forall_implies' {A : Type} {P : Prop} {Q : A → Prop} : (∀ x, (P → Q x)) ↔ (P → ∀ x, Q x) :=
    by
        constructor
        case mp =>
            intro h
            intro P
            intro x
            exact (h x) P
        case mpr =>
            intro h
            intro x
            intro P
            exact (h P) x

theorem paradoks_pivca {G : Type} {P : G → Prop} : (g : G) → ∃ (p : G), (P p → ∀ (x : G), P x) :=
    by
        intro g
        have h : (∀ (x : G), P x) ∨ ∃ (x : G), ¬P x := Classical.forall_or_exists_not P
        cases h with
        | inl hl =>
            exact ⟨g, imp_intro hl⟩
        | inr hr =>
            have hn : ¬∀ (x : G), P x := Classical.not_forall.mpr hr
            match hr with
            | ⟨p, hp⟩ => exact ⟨p, (iff_of_false hp hn).mp⟩

/------------------------------------------------------------------------------
 ## Dvojiška drevesa

 Podan naj bo tip dvojiških dreves skupaj s funkcijama za zrcaljenje in izračun
 višine ter dvema funkcijama, ki obe od leve proti desni naštejeta elemente
 drevesa. Pri tem prva deluje naivno in ima časovno zahtevnost O(n log n), druga
 pa je malo bolj zapletena in deluje v času O(n). Dokažite spodnje enakosti, pri
 čemer lahko do pomožne funkcije `aux` dostopate kot `elementi'.aux`
-------------------------------------------------------------------------------/

inductive Drevo : Type → Type where
    | prazno {A : Type} : Drevo A
    | sestavljeno {A : Type} : Drevo A → A → Drevo A → Drevo A

def zrcali {A : Type} : Drevo A → Drevo A :=
    fun (t : Drevo A) =>
        match t with
        | Drevo.prazno => Drevo.prazno
        | Drevo.sestavljeno l x d => Drevo.sestavljeno (zrcali d) x (zrcali l)

def visina {A : Type} : Drevo A → Nat :=
    fun (t : Drevo A) =>
        match t with
        | Drevo.prazno => 0
        | Drevo.sestavljeno l _ d => 1 + max (visina l) (visina d)

def elementi {A : Type} : Drevo A → List A :=
    fun (t : Drevo A) =>
        match t with
        | Drevo.prazno => []
        | Drevo.sestavljeno l x d => elementi l ++ x :: elementi d

def elementi' {A : Type} : Drevo A → List A :=
    let rec aux {A : Type} : Drevo A → List A → List A :=
        fun (t : Drevo A) (acc : List A) =>
            match t with
            | Drevo.prazno => acc
            | Drevo.sestavljeno l x d => aux l (x :: aux d acc)
    fun (t : Drevo A) =>
        aux t []

theorem zrcali_zrcali {A : Type} : (t : Drevo A) → zrcali (zrcali t) = t :=
    by
        intro t
        induction t with
        | prazno =>
            simp [zrcali]
        | sestavljeno l x d ihl ihd =>
            calc
                zrcali (zrcali (Drevo.sestavljeno l x d))
                _ = zrcali (Drevo.sestavljeno (zrcali d) x (zrcali l)) := by simp [zrcali]
                _ = Drevo.sestavljeno (zrcali (zrcali l)) x (zrcali (zrcali d)) := by simp [zrcali]
                _ = Drevo.sestavljeno l x d := by rw [ihl, ihd]

theorem visina_zrcali {A : Type} : (t : Drevo A) → visina (zrcali t) = visina t :=
    by
        intro t
        induction t with
        | prazno =>
            simp [zrcali, visina]
        | sestavljeno l x d ihl ihd =>
            calc
                visina (zrcali (Drevo.sestavljeno l x d))
                _ = visina (Drevo.sestavljeno (zrcali d) x (zrcali l)) := by simp [zrcali]
                _ = 1 + max (visina (zrcali d)) (visina (zrcali l)) := by simp [visina]
                _ = 1 + max (visina d) (visina l) := by rw [ihl, ihd]
                _ = 1 + max (visina l) (visina d) := by rw [Nat.max_comm]
                _ = visina (Drevo.sestavljeno l x d) := by simp [visina]

-- Pomožna trditev za dokaz enakosti funkcij iskanja elementov
theorem elementi'_aux_acc {A : Type} : (t : Drevo A) → (lst : List A) → (acc : List A) → elementi'.aux t acc ++ lst = elementi'.aux t (acc ++ lst) :=
    by
        intro t
        intro lst
        induction t with
        | prazno =>
            intro acc
            simp [elementi'.aux]
        | sestavljeno l x d ihl ihd =>
            intro acc
            calc
                elementi'.aux (Drevo.sestavljeno l x d) acc ++ lst
                _ = elementi'.aux l (x :: elementi'.aux d acc) ++ lst := by simp [elementi'.aux]
                _ = elementi'.aux l (x :: elementi'.aux d acc ++ lst) := ihl (x :: elementi'.aux d acc)
                _ = elementi'.aux l (x :: (elementi'.aux d acc ++ lst)) := by rw [List.cons_append x (elementi'.aux d acc) lst]
                _ = elementi'.aux l (x :: elementi'.aux d (acc ++ lst)) := by rw [ihd acc]
                _ = elementi'.aux (Drevo.sestavljeno l x d) (acc ++ lst) := by simp [elementi'.aux]

theorem elementi_elementi' {A : Type} : (t : Drevo A) → elementi t = elementi' t :=
    by
        intro t
        induction t with
        | prazno =>
            simp [elementi, elementi', elementi'.aux]
        | sestavljeno l x d ihl ihd =>
            calc
                elementi (Drevo.sestavljeno l x d)
                _ = elementi l ++ x :: elementi d := by simp [elementi]
                _ = elementi' l ++ x :: elementi' d := by rw [ihl, ihd]
                _ = elementi'.aux l [] ++ x :: elementi'.aux d [] := by simp [elementi']
                _ = elementi'.aux l (x :: elementi'.aux d []) := elementi'_aux_acc l (x :: elementi'.aux d []) []
                _ = elementi'.aux (Drevo.sestavljeno l x d) [] := by simp [elementi'.aux]
                _ = elementi' (Drevo.sestavljeno l x d) := by simp [elementi']
