memoize.html: memoize.lisp memoize.asd document
	./document

index.html: memoize.lisp memoize.html
	(cat memoize.html|sed -n '/<html/,/INTRODUCTION_PASTE/p'|head -n -1; \
        cat memoize.lisp|sed -n '/Commentary/,/Code/p'|cut -c4-|head -n -2|tail -n +3|markdown; \
        cat memoize.html|sed -n '/INTRODUCTION_PASTE/,$$p'|tail -n +2) > index.html; \
	rm memoize.html
