import tensorflow as tf
import numpy as np
from tensorflow.keras import models, callbacks
import os
import music21
from transformer_utils import (
    SinePositionEncoding,
    get_midi_note,
)

def causal_attention_mask(batch_size, n_dest, n_src, dtype):
    i = tf.range(n_dest)[:, None]
    j = tf.range(n_src)
    m = i >= j - n_src + n_dest
    mask = tf.cast(m, dtype)
    mask = tf.reshape(mask, [1, n_dest, n_src])
    mult = tf.concat(
        [tf.expand_dims(batch_size, -1), tf.constant([1, 1], dtype=tf.int32)], 0
    )
    return tf.tile(mask, mult)

class TransformerBlock(tf.keras.layers.Layer):
    def __init__(
        self,
        num_heads,
        key_dim,
        embed_dim,
        ff_dim,
        name=None,
        dropout_rate=0.1,
        **kwargs,
    ):
        # Accept standard Keras Layer kwargs (name, dtype, trainable, etc.)
        super(TransformerBlock, self).__init__(name=name, **kwargs)
        self.num_heads = num_heads
        self.key_dim = key_dim
        self.embed_dim = embed_dim
        self.ff_dim = ff_dim
        self.dropout_rate = dropout_rate
        self.attn = tf.keras.layers.MultiHeadAttention(
            num_heads, key_dim, output_shape=embed_dim
        )
        self.dropout_1 = tf.keras.layers.Dropout(self.dropout_rate)
        self.ln_1 = tf.keras.layers.LayerNormalization(epsilon=1e-6)
        self.ffn_1 = tf.keras.layers.Dense(self.ff_dim, activation="relu")
        self.ffn_2 = tf.keras.layers.Dense(self.embed_dim)
        self.dropout_2 = tf.keras.layers.Dropout(self.dropout_rate)
        self.ln_2 = tf.keras.layers.LayerNormalization(epsilon=1e-6)

    def call(self, inputs):
        input_shape = tf.shape(inputs)
        batch_size = input_shape[0]
        seq_len = input_shape[1]
        causal_mask = causal_attention_mask(
            batch_size, seq_len, seq_len, tf.bool
        )
        attention_output, attention_scores = self.attn(
            inputs,
            inputs,
            attention_mask=causal_mask,
            return_attention_scores=True,
        )
        attention_output = self.dropout_1(attention_output)
        out1 = self.ln_1(inputs + attention_output)
        ffn_1 = self.ffn_1(out1)
        ffn_2 = self.ffn_2(ffn_1)
        ffn_output = self.dropout_2(ffn_2)
        return (self.ln_2(out1 + ffn_output), attention_scores)

    def get_config(self):
        config = super().get_config()
        config.update(
            {
                "key_dim": self.key_dim,
                "embed_dim": self.embed_dim,
                "num_heads": self.num_heads,
                "ff_dim": self.ff_dim,
                "dropout_rate": self.dropout_rate,
            }
        )
        return config
    
class TokenAndPositionEmbedding(tf.keras.layers.Layer):
    def __init__(self, vocab_size, embed_dim, name=None, **kwargs):
        super(TokenAndPositionEmbedding, self).__init__(name=name, **kwargs)
        self.vocab_size = vocab_size
        self.embed_dim = embed_dim
        self.token_emb = tf.keras.layers.Embedding(
            input_dim=vocab_size,
            output_dim=embed_dim,
            embeddings_initializer="he_uniform",
        )
        self.pos_emb = SinePositionEncoding()

    def call(self, x):
        embedding = self.token_emb(x)
        positions = self.pos_emb(embedding)
        return embedding + positions

    def get_config(self):
        config = super().get_config()
        config.update(
            {
                "vocab_size": self.vocab_size,
                "embed_dim": self.embed_dim,
            }
        )
        return config
    
    # Create a MusicGenerator checkpoint
class MusicGenerator:
    def __init__(self, index_to_note, index_to_duration, model, top_k=10, callback_every=500):
        self.index_to_note = index_to_note
        self.note_to_index = {
            note: index for index, note in enumerate(index_to_note)
        }
        self.index_to_duration = index_to_duration
        self.duration_to_index = {
            duration: index for index, duration in enumerate(index_to_duration)
        }
        self.callback_every = callback_every
        self.model = model

    def sample_from(self, probs, temperature):
        probs = probs ** (1 / temperature)
        probs = probs / np.sum(probs)
        return np.random.choice(len(probs), p=probs), probs

    def get_note(self, notes, durations, temperature):
        sample_note_idx = 1
        while sample_note_idx == 1:
            sample_note_idx, note_probs = self.sample_from(
                notes[0][-1], temperature
            )
            sample_note = self.index_to_note[sample_note_idx]

        sample_duration_idx = 1
        while sample_duration_idx == 1:
            sample_duration_idx, duration_probs = self.sample_from(
                durations[0][-1], temperature
            )
            sample_duration = self.index_to_duration[sample_duration_idx]

        new_note = get_midi_note(sample_note, sample_duration)

        return (
            new_note,
            sample_note_idx,
            sample_note,
            note_probs,
            sample_duration_idx,
            sample_duration,
            duration_probs,
        )

    def generate(self, start_notes, start_durations, max_tokens, temperature):
        attention_model = models.Model(
            inputs=self.model.input,
            outputs=self.model.get_layer("attention").output,
        )

        start_note_tokens = [self.note_to_index.get(x, 1) for x in start_notes]
        start_duration_tokens = [
            self.duration_to_index.get(x, 1) for x in start_durations
        ]
        sample_note = None
        sample_duration = None
        info = []
        midi_stream = music21.stream.Stream()

        midi_stream.append(music21.clef.BassClef())

        for sample_note, sample_duration in zip(start_notes, start_durations):
            new_note = get_midi_note(sample_note, sample_duration)
            if new_note is not None:
                midi_stream.append(new_note)

        generated = 0
        while generated < max_tokens:
            x1 = np.array([start_note_tokens])
            x2 = np.array([start_duration_tokens])
            notes, durations = self.model.predict([x1, x2], verbose=0)

            repeat = True

            while repeat:
                (
                    new_note,
                    sample_note_idx,
                    sample_note,
                    note_probs,
                    sample_duration_idx,
                    sample_duration,
                    duration_probs,
                ) = self.get_note(notes, durations, temperature)

                if (
                    isinstance(new_note, music21.chord.Chord)
                    or isinstance(new_note, music21.note.Note)
                    or isinstance(new_note, music21.note.Rest)
                ) and sample_duration == "0.0":
                    repeat = True
                else:
                    repeat = False

            if new_note is not None:
                midi_stream.append(new_note)

            _, att = attention_model.predict([x1, x2], verbose=0)

            info.append(
                {
                    "prompt": [start_notes.copy(), start_durations.copy()],
                    "midi": midi_stream,
                    "chosen_note": (sample_note, sample_duration),
                    "note_probs": note_probs,
                    "duration_probs": duration_probs,
                    "atts": att[0, :, -1, :],
                }
            )
            start_note_tokens.append(sample_note_idx)
            start_duration_tokens.append(sample_duration_idx)
            start_notes.append(sample_note)
            start_durations.append(sample_duration)

            generated += 1

            if sample_note == "START":
                break

        return info
    
def compile_model(
        notes_vocab_size,
        durations_vocab_size,
        N_HEADS,
        KEY_DIM,
        EMBEDDING_DIM,
        FEED_FORWARD_DIM,
    ):
    note_inputs = tf.keras.layers.Input(shape=(None,), dtype=tf.int32)
    durations_inputs = tf.keras.layers.Input(shape=(None,), dtype=tf.int32)
    note_embeddings = TokenAndPositionEmbedding(
        notes_vocab_size, EMBEDDING_DIM // 2
    )(note_inputs)
    duration_embeddings = TokenAndPositionEmbedding(
        durations_vocab_size, EMBEDDING_DIM // 2
    )(durations_inputs)
    embeddings = tf.keras.layers.Concatenate()([note_embeddings, duration_embeddings])
    x, attention_scores = TransformerBlock(
        N_HEADS, KEY_DIM, EMBEDDING_DIM, FEED_FORWARD_DIM, name="attention"
    )(embeddings)
    note_outputs = tf.keras.layers.Dense(
        notes_vocab_size, activation="softmax", name="note_outputs"
    )(x)
    duration_outputs = tf.keras.layers.Dense(
        durations_vocab_size, activation="softmax", name="duration_outputs"
    )(x)
    model = tf.keras.models.Model(
        inputs=[note_inputs, durations_inputs],
        outputs=[note_outputs, duration_outputs],  # attention_scores
    )
    model.compile(
        "adam",
        loss=[
            tf.keras.losses.SparseCategoricalCrossentropy(),
            tf.keras.losses.SparseCategoricalCrossentropy(),
        ],
    )
    att_model = tf.keras.models.Model(
        inputs=[note_inputs, durations_inputs], outputs=attention_scores
    )

    return model, att_model