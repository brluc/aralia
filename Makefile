TARGET1=main
TARGET2=aralia
TARGET3=greek-table

book: $(TARGET1).rkt $(TARGET2).tex 
	racket -e '(require "$(TARGET1).rkt") (build-herbium)'
	pdflatex $(TARGET2)
	pdflatex $(TARGET2)

herbium: $(TARGET1).rkt
	racket $(TARGET1).rkt

greek: $(TARGET3).tex
	pdflatex $(TARGET3)
	pdflatex $(TARGET3)

test: alpha-sort.rkt
	raco test alpha-sort.rkt


.PHONY: clean
clean:
	rm -f *.aux *.log *.pdf *~
