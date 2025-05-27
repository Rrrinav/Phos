CXX := g++
CXXFLAGS := -Wall -Wextra -pedantic -std=c++2b -I./src
BUILD_FOLDER := bin
SOURCE_FOLDER := src

SOURCES := $(wildcard $(SOURCE_FOLDER)/*.cpp)
OBJECTS := $(patsubst $(SOURCE_FOLDER)/%.cpp, $(BUILD_FOLDER)/%.o, $(SOURCES))
TARGET := $(BUILD_FOLDER)/phos

# Default target
all: $(TARGET)

# Link the executable
$(TARGET): $(OBJECTS) | $(BUILD_FOLDER)
	$(CXX) $(CXXFLAGS) -o $@ $^

# Compile source files to object files
$(BUILD_FOLDER)/%.o: $(SOURCE_FOLDER)/%.cpp | $(BUILD_FOLDER)
	$(CXX) $(CXXFLAGS) -c $< -o $@

# Create build folder if missing
$(BUILD_FOLDER):
	mkdir -p $(BUILD_FOLDER)

# Clean build artifacts
clean:
	rm -rf $(BUILD_FOLDER)/*

.PHONY: all clean

