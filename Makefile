TARGET1=aralia
TARGET2=book

aralia: $(TARGET1).rkt 
	racket $(TARGET1).rkt
	pdflatex $(TARGET1)
	pdflatex $(TARGET1)

book: $(TARGET2).tex
	pdflatex $(TARGET2)
	pdflatex $(TARGET2)

.PHONY: clean
clean:
	rm -f *.aux *.log *.pdf *~
