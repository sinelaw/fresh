#!/usr/bin/env python3
"""Synthesize the 30 s score for the calm cut of the Fresh ad.

70 BPM, D major (the resolution of the loud cut's D minor). Rolled
electric-piano chords over a slow pad and a sine sub, a soft heartbeat kick
from bar 2, everything through a long convolution reverb. One bar = 3.43 s;
calm.js cuts on the same grid.

  Dmaj9 | Bm9 | Gmaj9 | A6sus -> A | x2, final Dmaj9 held and faded
"""
import sys
import wave

import numpy as np

SR = 44100
BPM = 70
BEAT = 60 / BPM
BAR = BEAT * 4
DUR = 30.0
N = int(SR * DUR)
rng = np.random.default_rng(3)
L = np.zeros(N)
R = np.zeros(N)


def mtof(m):
    return 440.0 * 2 ** ((m - 69) / 12)


def add(sig, t, gain=1.0, pan=0.0):
    i = int(t * SR)
    if i >= N:
        return
    sig = sig[: N - i]
    L[i:i + len(sig)] += sig * gain * (1 - max(pan, 0))
    R[i:i + len(sig)] += sig * gain * (1 + min(pan, 0))


def epiano(freq, length, vel=1.0):
    """Rhodes-ish: a sine carrier with a decaying 1:1 FM 'tine' and a soft bark."""
    n = int(length * SR)
    t = np.arange(n) / SR
    idx = 1.6 * vel * np.exp(-t / 0.18)
    mod = np.sin(2 * np.pi * freq * t)
    s = np.sin(2 * np.pi * freq * t + idx * mod)
    s += 0.18 * np.sin(2 * np.pi * freq * 2 * t) * np.exp(-t / 0.5)
    e = np.clip(t / 0.004, 0, 1) * (0.65 * np.exp(-t / 1.6) + 0.35 * np.exp(-t / 0.35))
    e[-2000:] *= np.linspace(1, 0, 2000)
    return s * e * vel


def pad(freqs, length, attack=1.6):
    n = int(length * SR)
    t = np.arange(n) / SR
    s = np.zeros(n)
    for f in freqs:
        for d in (-0.004, 0.0, 0.005):
            ph = rng.random()
            s += np.sin(2 * np.pi * (f * (1 + d) * t + ph))
            s += 0.25 * np.sin(2 * np.pi * (2 * f * (1 + d) * t + ph))
    s /= len(freqs) * 3
    e = np.clip(t / attack, 0, 1) * np.clip((length - t) / 1.2, 0, 1)
    return s * e


def sub(freq, length):
    n = int(length * SR)
    t = np.arange(n) / SR
    e = np.clip(t / 0.3, 0, 1) * np.clip((length - t) / 0.6, 0, 1)
    return np.sin(2 * np.pi * freq * t) * e


def soft_kick():
    n = int(0.6 * SR)
    t = np.arange(n) / SR
    f = 48 + 40 * np.exp(-t / 0.05)
    return np.sin(2 * np.pi * np.cumsum(f) / SR) * np.exp(-t / 0.25)


def shaker():
    n = int(0.12 * SR)
    t = np.arange(n) / SR
    x = rng.standard_normal(n)
    x = np.diff(x, prepend=0)
    return x * np.clip(t / 0.02, 0, 1) * np.exp(-t / 0.035)


# (root for bass, voicing as MIDI notes)
CHORDS = [
    (38, [62, 66, 69, 73, 76]),      # Dmaj9   D F# A C# E
    (35, [59, 62, 66, 69, 73]),      # Bm9     B D F# A C#
    (43, [59, 62, 66, 69, 71]),      # Gmaj9   (B D F# A + B on top)
    (45, [57, 62, 64, 66, 69]),      # A6sus   A D E F# A
]
MELODY = {  # bar -> [(beat, midi)] a sparse top line
    2: [(0, 81), (1.5, 78), (3, 76)],
    3: [(0, 78), (2, 74)],
    4: [(0, 76), (1.5, 74), (2.5, 73)],
    5: [(0, 73), (2, 76)],
    6: [(0, 81), (1.5, 83), (3, 81)],
    7: [(0, 78), (2, 76), (3, 73)],
}

nbars = int(np.ceil(DUR / BAR))
for bar in range(nbars):
    t0 = bar * BAR
    final = bar >= 8
    root, voicing = CHORDS[0] if final else CHORDS[bar % 4]
    length = DUR - t0 if final else BAR + 1.2
    # rolled chord on beat 1, a softer re-strike on beat 3
    for i, m in enumerate(voicing):
        add(epiano(mtof(m), min(length, 4.5), 0.8), t0 + i * 0.035, 0.3, pan=(i - 2) * 0.18)
        if not final and bar >= 1:
            add(epiano(mtof(m), 2.5, 0.45), t0 + 2 * BEAT + i * 0.03, 0.17, pan=(2 - i) * 0.18)
    add(pad([mtof(m) for m in voicing[:4]], length), t0, 0.3)
    add(sub(mtof(root), length), t0, 0.17)
    # a low piano root an octave up, so the bass reads on phone speakers too
    add(epiano(mtof(root + 12), min(length, 4.0), 0.6), t0, 0.26)
    for beat, m in MELODY.get(bar, []):
        add(epiano(mtof(m), 3.0, 0.7), t0 + beat * BEAT, 0.3, pan=0.25)
    if 1 <= bar < 8:
        for beat in (0, 2):
            add(soft_kick(), t0 + beat * BEAT, 0.16)
        for e8 in range(8):
            add(shaker(), t0 + e8 * BEAT / 2 + 0.012, 0.05 if e8 % 2 else 0.028, pan=0.35)

# reverb: exponentially decaying stereo noise impulse, convolved via FFT
ir_len = int(2.8 * SR)
tt = np.arange(ir_len) / SR
irL = rng.standard_normal(ir_len) * np.exp(-tt / 0.9)
irR = rng.standard_normal(ir_len) * np.exp(-tt / 0.9)
irL[: int(0.02 * SR)] = 0
irR[: int(0.025 * SR)] = 0


def conv(x, ir):
    n = len(x) + len(ir)
    size = 1 << (n - 1).bit_length()
    y = np.fft.irfft(np.fft.rfft(x, size) * np.fft.rfft(ir, size), size)[: len(x)]
    return y / np.sqrt(np.sum(ir ** 2))


wetL, wetR = conv(L, irL), conv(R, irR)
mixL = L * 0.75 + wetL * 0.35
mixR = R * 0.75 + wetR * 0.35
mix = np.stack([mixL, mixR], axis=1)
t = np.arange(N) / SR
fade = np.clip(t / 1.5, 0, 1) * np.clip((DUR - t) / 3.0, 0, 1)
mix *= fade[:, None]
mix = np.tanh(mix * 1.1)
mix /= np.max(np.abs(mix)) / 0.8

out = sys.argv[1] if len(sys.argv) > 1 else "music_calm.wav"
with wave.open(out, "wb") as w:
    w.setnchannels(2)
    w.setsampwidth(2)
    w.setframerate(SR)
    w.writeframes((mix * 32767).astype("<i2").tobytes())
print("wrote", out)
