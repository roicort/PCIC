# Gradio app para generación musical con Transformer

import os
import sys
import gradio as gr
from regex import T
import tensorflow as tf
from torch import seed
from transformer_utils import (
    load_parsed_files,
)
from app_utils import MusicGenerator, compile_model
from music21 import environment
from PIL import Image

# Parámetros y rutas
MODEL_PATH = "./models/model.keras"
PARSED_DATA_PATH = "./parsed_data/"

us = environment.UserSettings()

if sys.platform.startswith("darwin"):
    us['musicxmlPath'] = '/Applications/MuseScore 4.app/Contents/MacOS/mscore'
    us['musescoreDirectPNGPath'] = '/Applications/MuseScore 4.app/Contents/MacOS/mscore'
    SAVE_PATH = os.path.join(os.getcwd(), "app_tmp")
if sys.platform.startswith("linux"):
    us['musicxmlPath'] = '/usr/bin/musescore'
    us['musescoreDirectPNGPath'] = '/usr/bin/musescore'
    SAVE_PATH = os.path.join('/tmp', 'app_tmp')

# Save Path
os.makedirs(SAVE_PATH, exist_ok=True)

SEQ_LEN = 50
EMBEDDING_DIM = 256
KEY_DIM = 256
N_HEADS = 5
DROPOUT_RATE = 0.3
FEED_FORWARD_DIM = 256
BATCH_SIZE = 256

def create_dataset(elements):
    ds = (
        tf.data.Dataset.from_tensor_slices(elements)
        .batch(BATCH_SIZE, drop_remainder=True)
        .shuffle(1000)
    )
    vectorize_layer = tf.keras.layers.TextVectorization(
        standardize=None, output_mode="int"
    )
    vectorize_layer.adapt(ds)
    vocab = vectorize_layer.get_vocabulary()
    return ds, vectorize_layer, vocab

# Cargar datos parseados y construir vocabularios
notes, durations = load_parsed_files(PARSED_DATA_PATH)
notes_seq_ds, notes_vectorize_layer, notes_vocab = create_dataset(notes)
durations_seq_ds, durations_vectorize_layer, durations_vocab = create_dataset(
    durations
)
notes_vocab_size = len(notes_vocab)
durations_vocab_size = len(durations_vocab)

# Cargar modelo

SEQ_LEN = 50
EMBEDDING_DIM = 256
KEY_DIM = 256
N_HEADS = 5
DROPOUT_RATE = 0.3
FEED_FORWARD_DIM = 256
LOAD_MODEL = True

model, attention_model = compile_model(
    notes_vocab_size=len(notes_vocab),
    durations_vocab_size=len(durations_vocab),
    N_HEADS=N_HEADS,
    KEY_DIM=KEY_DIM,
    EMBEDDING_DIM=EMBEDDING_DIM,
    FEED_FORWARD_DIM=FEED_FORWARD_DIM,
)

model.load_weights(MODEL_PATH)
music_generator = MusicGenerator(notes_vocab, durations_vocab, model)

# Gradio UI

def generate_music(seed_note, seed_duration, length, temperature):
    info = music_generator.generate(
        seed_note.split(),
        seed_duration.split(),
        max_tokens=length,
        temperature=temperature
    )

    midi_stream = info[-1]["midi"].chordify()

    imagepath = os.path.join(SAVE_PATH, "generated_music.png")
    midi_stream.write('musicxml.png', fp=imagepath)
    musicsheetpath = imagepath.replace('.png', '-1.png')
    musicimg = Image.open(musicsheetpath)

    midipath = os.path.join(SAVE_PATH, "generated_music.mid")
    midi_stream.write('midi', fp=midipath)

    parsed_midi = midi_stream.flatten().notes
    midi_stream_repr = ""
    for element in parsed_midi:
        if element.isNote:
            midi_stream_repr += f"NOTE {element.pitch} DURATION {element.quarterLength}\n"
        elif element.isChord:
            pitches = '.'.join(str(n) for n in element.pitches)
            midi_stream_repr += f"CHORD {pitches} DURATION {element.quarterLength}\n"

    return musicimg, midipath, midi_stream_repr

iface = gr.Interface(
    fn=generate_music,
    inputs=[
        gr.Textbox(label="Seed Note", value="START G:major 4/4TS"),
        gr.Textbox(label="Seed Duration", value="0.0 0.0 0.0"),
        gr.Slider(label="Length (tokens)", minimum=10, maximum=500, step=10, value=100),
        gr.Slider(label="Temperature", minimum=0.1, maximum=1.0, step=0.1, value=0.5),
    ],
    outputs=[
        gr.Image(label="Generated Music Sheet"),
        gr.File(label="Descargar MIDI"),
        gr.Textbox(label="MIDI Stream Representation"),
    ],
    title="Music Transformer Generator",
    description="Genera música utilizando un modelo Transformer entrenado.",
    flagging_mode="never",
)

iface.launch()