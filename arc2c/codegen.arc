;------------------------------------------------------------------------------

; code generation

(def code-generate (ast)
	(withs
		(lambda-todo '()
		 lambda-count 0
		 global-vars (fv ast)
		 add-lambda [let i lambda-count (push (cons lambda-count _) lambda-todo) (++ lambda-count) i])

	(= code-gen (fn (ast stack-env)
		(= cg-list (fn (asts vars stack-env sep k)
			(if (no asts)
				(k "" stack-env)
				(let x (code-gen (car asts) stack-env)
					(cg-list (cdr asts) (cdr vars) (cons (car vars) stack-env) sep (fn (code stack-env) (k (list x sep code) stack-env)))))))

		(= cg-args (fn (args stack-env)
			(cg-list args (range 1 (len args)) stack-env "" (fn (code stack-env) code))))

		(= access-var (fn (var stack-env)
			(if (aglobal var)
				(let i (pos var global-vars)
					(list "GLOBAL(" i "/*" var!uid "*/)"))
				(let i (- (len stack-env) (pos var stack-env) 1)
					(list "LOCAL(" i "/*" var!uid "*/)")))))

		(= cg (fn (ast)
			(if
				(alit ast)
					(let val ast!val
						(if
							(no val) (list " PUSH(NILOBJ);")
							(is val t) (list " PUSH(TOBJ);")
							(list " PUSH(FIX2OBJ(" val "));")))
				(aquote ast)
					(list " PUSH(SYM2OBJ(\"" (car ast!subx) "\"));")
				(aref ast)
					(list " PUSH(" (access-var ast!var stack-env) ");")
				(aset ast)
					(list (cg (car ast!subx)) " " (access-var ast!var stack-env) " = TOS();")
				(acnd ast)
					(let x (map cg ast!subx)
						(list (car x) "\n if (POP() != NILOBJ) {\n" (cadr x) "\n } else {\n" (car:cddr x) "\n }"))
				(aprim ast)
					(let args ast!subx
						(if
							(is ast!op '%is) (list (cg-args args stack-env) " EQ();")
							(is ast!op '%isnt) (list (cg-args args stack-env) " NEQ();")
							(is ast!op '%<) (list (cg-args args stack-env) " LT();")
							(is ast!op '%>) (list (cg-args args stack-env) " GT();")
							(is ast!op '%<=) (list (cg-args args stack-env) " LE();")
							(is ast!op '%>=) (list (cg-args args stack-env) " GE();")
							(is ast!op '%+) (list (cg-args args stack-env) " ADD();")
							(is ast!op '%-) (list (cg-args args stack-env) " SUB();")
							(is ast!op '%*) (list (cg-args args stack-env) " MUL();")
							(is ast!op '%prn) (list (cg-args args stack-env) " DISPLAY();")
							(is ast!op '%halt) (list (cg-args args stack-env) " HALT();")
							(is ast!op '%closure)
								(withs
									(i (add-lambda (car args))
									 n (len (cdr args))
									 s (list "CLOSURE(" i "," n ");"))
									(list (cg-args (cdr args) stack-env) " BEGIN_" s (map [list " INICLO(" _ ");"] (rev:range 1 n)) " END_" s))
							(is ast!op '%closure-ref)
								(let i ((cadr args) 'val)
									(list (cg (car args)) " TOS() = CLOSURE_REF(TOS()," i ");"))
							(err "unknown primitive" ast!op)))
				(anapp ast)
					(withs
						(fun (car ast!subx)
						 args (cdr ast!subx)
						 n (len args))
						(if (alam fun)
							(cg-list args fun!params stack-env "\n" (fn (code new-stack-env) (list code (code-gen (car fun!subx) new-stack-env))))
							(cg-list args (range 1 n) stack-env "\n" (fn (code new-stack-env)
								(with
									(start (len stack-env)
									 s (list "JUMP(" n ");"))
									(list code " BEGIN_" s (map [list " PUSH(LOCAL(" (+ _ start) "));"] (range 0 (- n 1))) " END_" s))))))
				(alam ast) ; this case is impossible after CPS-conversion
					(list " PUSH(FIX2OBJ(" (add-lambda ast) "));")
				(aseq ast) ; this case is impossible after CPS-conversion
					(map [list (cg _) "DROP();"] ast!subx)
					(err "unknown ast" ast))))

		(cg ast)))

	(= compile-all-lambdas (fn ()
		(if (no lambda-todo)
			""
			(withs
				(x (car lambda-todo)
				 ast (cdr x))
				(pop lambda-todo)
					(list
						"case " (car x) ":\n\n"
						(code-gen (car ast!subx) (rev ast!params))
						"\n\n"
						(compile-all-lambdas))))))

	(add-lambda ast)

	(let code (compile-all-lambdas)
		(list
			(list	"#define NB_GLOBALS " (len global-vars) "\n"	"#define MAX_STACK " 100000 "\n"	code-prefix*)
			code
			code-suffix*))))

(= code-prefix* "
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define HEAP_SIZE 100000000
#define MAX_SYMS 1000000

typedef int obj;

obj global[NB_GLOBALS];
obj stack[MAX_STACK];
obj heap[HEAP_SIZE];
char * syms[MAX_SYMS];
int nsyms;

#define AFIX(o) (((o) & 1) == 0) /* the last bit is 0 : it's a fixnum */
#define ASYM(o) (((o) & 3) == 1) /* last 2 bits are 01 : it's a symbol */

#define FIX2OBJ(n) ((n) << 1)
#define OBJ2FIX(o) ((o) >> 1)

#define PTR2OBJ(p) ((obj)(p) + 1)
#define OBJ2PTR(o) ((obj*)((o) - 1)) 

#define NILOBJ 1 /* 0...0001 */ 
#define TOBJ   5 /* 0...0101 */

#define OBJ2SYM(o) (syms[(o) >> 2])

/* Other symbols will be ending with 1001 ; the most-significant bits designate a ref */
/* Other values (conses, tables, strings, bignums, ...) will be ending with 11 ; most-significant bits represent a ref */

#define GLOBAL(i) global[i]
#define LOCAL(i) stack[i]
#define CLOSURE_REF(self,i) OBJ2PTR(self)[i]

#define TOS() sp[-1]
#define PUSH(x) *sp++ = x
#define POP() *--sp

#define EQ() { obj y = POP(); TOS() = TOS() == y ? TOBJ : NILOBJ; }
#define NEQ() { obj y = POP(); TOS() = TOS() != y ? TOBJ : NILOBJ; }
#define LT() { obj y = POP(); TOS() = TOS() < y ? TOBJ : NILOBJ; }
#define GT() { obj y = POP(); TOS() = TOS() > y ? TOBJ : NILOBJ; }
#define LE() { obj y = POP(); TOS() = TOS() <= y ? TOBJ : NILOBJ; }
#define GE() { obj y = POP(); TOS() = TOS() >= y ? TOBJ : NILOBJ; }
#define ADD() { obj y = POP(); TOS() = TOS() + y; }
#define SUB() { obj y = POP(); TOS() = TOS() - y; }
#define MUL() { obj y = POP(); TOS() = OBJ2FIX(TOS()) * y; }
#define DISPLAY() { obj y = TOS(); if (AFIX(y)) printf (\"%d\\n\", OBJ2FIX(y)); else printf (\"%s\\n\", OBJ2SYM(y)); }
#define HALT() break

#define BEGIN_CLOSURE(label,nbfree) if (hp-(nbfree+1) < heap) hp = gc (sp);
#define INICLO(i) *--hp = POP()
#define END_CLOSURE(label,nbfree) *--hp = label; PUSH(PTR2OBJ(hp));

#define BEGIN_JUMP(nbargs) sp = stack;
#define END_JUMP(nbargs) pc = OBJ2PTR(LOCAL(0))[0]; goto jump;

obj SYM2OBJ (char * s){ /* Find a symbol, or save it if it's the first time */
	int i;

	for (i = 0 ; i < nsyms ; i++)
		if (strcmp (s, syms[i]) == 0) /* found it */
			return (i << 2) + 1;

	if (nsyms == MAX_SYMS){ /* Bad luck, really... */
		fprintf (stderr, \"Sorry, we just ran out of symbols. Please come back later...\\n\");
		exit (1);
	}

	syms[nsyms] = (char *) malloc (strlen (s) + 1); /* free ? come on, who needs free nowadays, with all this cheap RAM ? */
	strcpy (syms[nsyms], s);
	nsyms++;

	return ((nsyms - 1) << 2) + 1;
}

obj *gc (obj *sp){
	fprintf (stderr, \"Well, sorry about it but I ran out of memory and I don't know how to clean it up yet, so I'll just go out... Bye...\\n\");
	exit (1);
}

obj execute (void)
{
  int pc = 0;
  obj *sp = stack;
  obj *hp = &heap[HEAP_SIZE];

  jump: switch (pc) {

")

(= code-suffix* "  }
  return POP();
}

int main (int argc, char * argv[]) {
	syms[0] = (char *) malloc (strlen (\"nil\") + 1);
	syms[1] = (char *) malloc (strlen (\"t\") + 1);
	strcpy (syms[0], \"nil\");
	strcpy (syms[1], \"t\");
	nsyms = 2;

	printf (\"This Arc program returned the following object id : %d\\n\", (int) execute());
	return 0;
}
")

;------------------------------------------------------------------------------

; debugging

(def source (ast)
	(if
		(alit ast)
			 ast!val
		(aref ast)
			((ast 'var) 'uid)
		(aset ast)
			(list 'set ((ast 'var) 'uid) (source (car ast!subx)))
		(acnd ast)
			(cons 'if (map source ast!subx))
		(aprim ast)
			(cons ast!op (map source ast!subx))
		(anapp ast)
			(if (alam (car ast!subx))
				(list 'let (map (fn (p a) (list p!uid (source a))) ((car ast!subx) 'params) (cdr ast!subx)) (source (car ((car ast!subx) 'subx))))
				(map source ast!subx))
		(alam ast)
			(list 'fn (map [_ 'uid] ast!params) (source (car ast!subx)))
		(aseq ast)
			(cons 'do (map source ast!subx))
		(aquote ast)
			(cons 'quote ast!subx)
			(err "unknown ast" ast)))

(def ds (ast)
   (if
      (alit ast)
          (cons 'lit ast!val)
      (aref ast)
         (cons 'ref ((ast 'var) 'uid))
      (aset ast)
         (cons 'set (list 'set ((ast 'var) 'uid) (ds (car ast!subx))))
      (acnd ast)
         (cons 'cnd (cons 'if (map ds ast!subx)))
      (aprim ast)
         (cons 'prim (cons ast!op (map ds ast!subx)))
      (anapp ast)
			(cons 'app
         (if (alam (car ast!subx))
            (list 'let (map (fn (p a) (list p!uid (ds a))) ((car ast!subx) 'params) (cdr ast!subx)) (ds (car ((car ast!subx) 'subx))))
            (map ds ast!subx)))
      (alam ast)
         (cons 'lam (list 'fn (map [_ 'uid] ast!params) (ds (car ast!subx))))
      (aseq ast)
         (cons 'seq (cons 'do (map ds ast!subx)))
         (err "unknown ast" ast)))


;------------------------------------------------------------------------------

(def strip-ext (filename)
	(cut filename 0 (pos #\. filename)))

(def compile-file (filename)
	(let ast (parse-file filename)
		(prn "-------------------------- AST:")
		(prn (source ast))

		(let ast-after-cps (cps-convert ast)
			(prn "-------------------------- AST AFTER CPS-CONVERSION:")
			(prn (source ast-after-cps))

			(let ast-after-cc (closure-convert ast-after-cps)
				(prn "-------------------------- AST AFTER CLOSURE-CONVERSION:")
				(prn (source ast-after-cc))

				(let code (code-generate ast-after-cc)
					(prn "-------------------------- C CODE:")
					;(prn (cadr code))
					(w/outfile f (+ (strip-ext filename) ".c")
						(w/stdout f
							(prn (liststr code)))))))))

