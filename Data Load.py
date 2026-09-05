# -*- coding: utf-8 -*-
"""
Created on Sun Oct 26 20:31:37 2025

@author: naili
"""

import pandas as pd
import mido

# common path from every file
base_path = "groove/"
metadata = pd.read_csv(base_path + "info.csv")
print(f"Found {len(metadata)} MIDI files to process\n")

# Extracting all hits from all files
all_hits = []

for idx, row in metadata.iterrows():
    if idx % 50 == 0:
        print(f"Processing file {idx}/{len(metadata)}...")

# attaching every specific file name to the base path
    try:
        midi_path = base_path + row['midi_filename']
        midi_file = mido.MidiFile(midi_path)
        
        current_time = 0
        
        for msg in midi_file:
            current_time += msg.time
            if msg.type == 'note_on' and msg.velocity > 0 and msg.channel == 9:
                all_hits.append({
                    'drummer': row['drummer'],
                    'session': row['session'],
                    'id': row['id'],
                    'style': row['style'],
                    'bpm': row['bpm'],
                    'beat_type': row['beat_type'],
                    'time_signature': row['time_signature'],
                    'time': current_time,
                    'velocity': msg.velocity,
                    'pitch': msg.note
                })
                
    except Exception as e:
        print(f"Error processing {row['midi_filename']}: {e}")

# Create DataFrame
print("\nCreating DataFrame...")
df = pd.DataFrame(all_hits)

# Sort and calculate inter-onset intervals
print("Calculating inter-onset intervals...")
df = df.sort_values(['drummer', 'session', 'id', 'time']).reset_index(drop=True)
df['inter_onset_interval'] = df.groupby(['drummer', 'session', 'id'])['time'].diff()


output_path = "all_hits.csv""
df.to_csv(output_path, index=False)

print(f"\n{'='*50}")
print(f"SUCCESS! Processed {len(df):,} drum hits")
print(f"Saved to: {output_path}")
print(f"{'='*50}\n")

print("Hits per drummer:")
print(df['drummer'].value_counts().sort_index())

print("\nSample of data:")
print(df.head(10))

