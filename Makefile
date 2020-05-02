TARGET1=herbium-gen-latex
TARGET2=aralia
TARGET3=greek-table

book: $(TARGET1).rkt $(TARGET2).tex 
	racket $(TARGET1).rkt
	pdflatex $(TARGET2)
	pdflatex $(TARGET2)

herbium: $(TARGET1).rkt
	racket $(TARGET1).rkt

greek: $(TARGET3).tex
	pdflatex $(TARGET3)
	pdflatex $(TARGET3)

.PHONY: clean
clean:
	rm -f *.aux *.log *.pdf *~
