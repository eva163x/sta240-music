library(gm)
# ===============================================
# Set up available pitches
# ===============================================

pitches_base <- c("C", "D", "E", "F", "G", "A", "B")
octaves <- 4:5
all_pitches <- paste(rep(pitches_base, length(octaves)),
                     sort(rep(octaves, length(pitches_base))), sep = "")

# ===============================================
# Chords
# ===============================================

# I–V–vi–IV chord progression (C major)
# C–G–Am–F
chords <- list(
  c("C3", "E3", "G3"),    # I
  c("G2", "B2", "D3"),    # V
  c("A2", "C3", "E3"),    # vi
  c("F2", "A2", "C3")     # IV
)

# ===============================================
# Melody
# ===============================================

melody_notes <- sample(all_pitches, 64, replace=TRUE)
melody_durations <- sample(c(0.5, 1, 2), size=64, replace=TRUE, prob=c(0.4, 0.4, 0.2))

#total duration of the melody
total_melody_duration <- sum(melody_durations)

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
# Setup (customize music settings)
# ===============================================

set.seed(42)
tempo <- sample(90:110, 1)  # Pop tempo
meter <- c(4, 4)
key <- 0 #for C major, no sharps/flats

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

#chord duration should match melody duration...
current_chord_duration <- 0
chord_index <- 1
all_chord_pitches <- list()
all_chord_durations <- numeric()

while (current_chord_duration < total_melody_duration) {
  # adding the current chord from the progression
  all_chord_pitches[[length(all_chord_pitches) + 1]] <- chords[[chord_index]]
  
  # what is duration for this chord.
  chord_duration_to_add <- 4 #4 beats per chord for now
  
  #pls work-- we shouldn't go over the total melody duration
  if (current_chord_duration + chord_duration_to_add > total_melody_duration) {
    chord_duration_to_add <- total_melody_duration - current_chord_duration
  }
  
  all_chord_durations <- c(all_chord_durations, chord_duration_to_add)
  current_chord_duration <- current_chord_duration + chord_duration_to_add
  
  # move tp next chord in the progression, looping if necessary
  chord_index <- (chord_index %% length(chords)) + 1
}

#adding the chords to the music
music <- music +
  Line(pitches = all_chord_pitches,
       durations = all_chord_durations) +
  Instrument(1) # piano

# melody
music <- music +
  Line(pitches = melody_notes,
       durations = melody_durations) +
  Instrument(1) # violin


# ===============================================
# Show the music!
# ===============================================

show(music, to = "audio")