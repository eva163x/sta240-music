library(gm)

#does my git work??

#hiii

# ===============================================
# Set up available pitches
# ===============================================

pitches_base <- c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")
octaves <- 3:5
all_pitches <- paste(rep(pitches_base, length(octaves)), 
                     sort(rep(octaves, length(pitches_base))), sep = "")

# ===============================================
# random walk on the elements of a vector
# ===============================================

random_walk <- function(vec, start_pos, steps) {
  n <- length(vec)
  
  # Initialize walk
  positions <- numeric(steps + 1)
  positions[1] <- start_pos
  
  for (i in 2:(steps + 1)) {
    current_pos <- positions[i - 1]
    
    # At the left boundary
    if (current_pos == 1) {
      new_pos <- 2
    }
    # At the right boundary
    else if (current_pos == n) {
      new_pos <- n - 1
    }
    # Otherwise move left or right with equal probability
    else {
      direction <- sample(c(-1, 1), size = 1)
      new_pos <- current_pos + direction
    }
    positions[i] <- new_pos
  }
  return(vec[positions])
}

# ===============================================
# Setup
# ===============================================

set.seed(123)

meter_options <- list(c(3, 4), c(4, 4), c(6, 8)) # 3/4, 4/4, or 6/8

meter <- sample(meter_options, 1)[[1]]

tempo <- sample(80:120, 1)  # BPM between 80 and 120

key <- sample(-7:7, 1)  # Key from 7 sharps to 7 flats

instrument_choices <- c(1, 5, 10, 20, 40, 41, 42, 72, 74)  # various MIDI instruments

melody_notes <- random_walk(all_pitches, start_pos = sample(1:length(all_pitches), 1), steps = 31)

durations <- sample(c(0.5, 1, 1.5, 2), size = length(melody_notes), replace = TRUE)

# ===============================================
# Music settings
# ===============================================

my_meter <- c(4, 4)  # 4 beats per bar
my_tempo <- 100      # bpm
my_key <- 0          # 0 sharps/flats (A minor shares key signature with C major)

# ===============================================
# Chords
# ===============================================

chords <- list(
  c("A3", "C4", "E4"),   # A minor
  c("D3", "F3", "A3"),   # D minor
  c("G3", "B3", "D4"),   # G major
  c("A3", "C4", "E4")    # A minor again
)

# ===============================================
# Start the music
# ===============================================

music <- Music() + 
  Meter(meter[1], meter[2]) + 
  Tempo(tempo) + 
  Key(key)

# ===============================================
# Build random music lines
# ===============================================

for (i in 1:10) {  # I made 10 loops for faster testing
  music <- music + 
    Line(pitches = sample(melody_notes, 20, replace = TRUE),
         durations = sample(c(2, 1, 0.5, 0.25), 20, replace = TRUE)) + 
    Instrument(sample(instrument_choices, 1))
}

# ===============================================
# Show the music!
# ===============================================

show(music, to = "audio")
