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
import random
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

# Función para randomizar seed notes y durations
def randomize_seeds():
    # Selecciona escala y tempo aleatorios del vocabulario de notas
    escala = random.choice([n for n in notes_vocab if 'major' in n or 'minor' in n]) if any('major' in n or 'minor' in n for n in notes_vocab) else 'G:major'
    tempo = random.choice([n for n in notes_vocab if 'TS' in n]) if any('TS' in n for n in notes_vocab) else '4/4TS'
    notas_puras = [n for n in notes_vocab if 'major' not in n and 'minor' not in n and 'TS' not in n and n != 'START']
    notas_aleatorias = ' '.join(random.choices(notas_puras, k=3))
    duraciones_aleatorias = ' '.join(random.choices(durations_vocab, k=3))
    seed_notes = f"START {escala} {tempo} {notas_aleatorias}"
    seed_durations = f"0.0 0.0 0.0 {duraciones_aleatorias}"
    return seed_notes, seed_durations

with gr.Blocks() as demo:
    gr.Markdown("# Music Transformer Generator\nGenera música utilizando un modelo Transformer pre-entrenado.")
    with gr.Row():
        seed_note = gr.Textbox(label="Seed Note", value="START G:major 4/4TS")
        seed_duration = gr.Textbox(label="Seed Duration", value="0.0 0.0 0.0")
        random_btn = gr.Button("Randomize Seeds")
    length = gr.Slider(label="Length (tokens)", minimum=10, maximum=500, step=10, value=100)
    temperature = gr.Slider(label="Temperature", minimum=0.1, maximum=1.0, step=0.1, value=0.5)
    with gr.Row():
        music_img = gr.Image(label="Generated Music Sheet")
        midi_file = gr.File(label="Descargar MIDI")
        midi_stream_repr = gr.Textbox(label="MIDI Stream Representation")

    generate_btn = gr.Button("Generar Música")

    def update_seeds():
        n, d = randomize_seeds()
        return n, d

    random_btn.click(update_seeds, [], [seed_note, seed_duration])
    generate_btn.click(
        generate_music,
        [seed_note, seed_duration, length, temperature],
        [music_img, midi_file, midi_stream_repr]
    )

demo.launch()