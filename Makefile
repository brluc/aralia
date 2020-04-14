TARGET1=aralia

all: $(TARGET1).rkt 
	racket $(TARGET1).rkt
	pdflatex $(TARGET1)
	pdflatex $(TARGET1)

.PHONY: clean
clean:
	rm -f *.aux *.log *.pdf *~
