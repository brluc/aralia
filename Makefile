TARGET1=herbium
TARGET2=aralia
TARGET3=greek-table

aralia: $(TARGET1).rkt 
	racket $(TARGET1).rkt
	pdflatex $(TARGET2)
	pdflatex $(TARGET2)

gk: $(TARGET3).tex
	pdflatex $(TARGET3)
	pdflatex $(TARGET3)

.PHONY: clean
clean:
	rm -f *.aux *.log *.pdf *~
