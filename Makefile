SCALAC = scalac
SCALA = scala
TARGET = CourseFunctions

all: run clean

compile:
	$(SCALAC) $(TARGET).scala

run: compile
	$(SCALA) $(TARGET)

clean:
	rm -f *.class
