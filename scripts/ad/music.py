#!/usr/bin/env python3
"""Synthesize a 30s, 128 BPM electronic track for the Fresh ad.

Bar grid (1 bar = 1.752s) matches the cut list in ad.html:
  bar 0      hook: three stabs + riser
  bars 1-11  groove (drop at 1.875s)
  bars 12-13 feature blitz: 16th hats, snare roll, riser
  bars 14-15 end card: impact, pad, fade
"""
import sys
import wave

import numpy as np

SR = 44100
BPM = 137
BEAT = 60 / BPM
BAR = BEAT * 4
DUR = 30.0
N = int(SR * DUR)
rng = np.random.default_rng(7)

L = np.zeros(N)
R = np.zeros(N)


def add(sig, t, gain=1.0, pan=0.0):
    i = int(t * SR)
    if i >= N:
        return
    sig = sig[: N - i]
    L[i:i + len(sig)] += sig * gain * (1 - max(pan, 0))
    R[i:i + len(sig)] += sig * gain * (1 + min(pan, 0))


def env(n, attack=0.002, decay=0.2):
    t = np.arange(n) / SR
    a = np.clip(t / attack, 0, 1)
    return a * np.exp(-t / decay)


def onepole_lp(x, cutoff):
    cutoff = np.broadcast_to(cutoff, x.shape)
    y = np.zeros_like(x)
    acc = 0.0
    k = 1 - np.exp(-2 * np.pi * cutoff / SR)
    for i in range(len(x)):
        acc += k[i] * (x[i] - acc)
        y[i] = acc
    return y


def kick(level=1.0):
    n = int(0.45 * SR)
    t = np.arange(n) / SR
    f = 52 + 110 * np.exp(-t / 0.03)
    ph = 2 * np.pi * np.cumsum(f) / SR
    s = np.sin(ph) * np.exp(-t / 0.16)
    click = rng.standard_normal(n) * np.exp(-t / 0.003) * 0.3
    return np.tanh((s + click) * 1.6) * level * 0.6


def clap():
    n = int(0.3 * SR)
    t = np.arange(n) / SR
    noise = rng.standard_normal(n)
    noise = noise - onepole_lp(noise, 900)  # highpass-ish
    e = np.zeros(n)
    for d in (0, 0.011, 0.022):
        e += np.where(t >= d, np.exp(-(t - d) / 0.012), 0)
    e += np.where(t >= 0.03, np.exp(-(t - 0.03) / 0.09), 0)
    return noise * e * 0.5


def hat(decay=0.035):
    n = int(0.15 * SR)
    noise = rng.standard_normal(n)
    hp = np.diff(np.diff(noise, prepend=0), prepend=0)
    return hp * env(n, 0.0005, decay) * 0.18


def saw(freq, n, detune=0.0):
    t = np.arange(n) / SR
    out = np.zeros(n)
    for d in (-detune, 0, detune):
        ph = (t * freq * (1 + d)) % 1.0
        out += 2 * ph - 1
    return out / 3


def bass_note(freq, length):
    n = int(length * SR)
    s = saw(freq, n, 0.004) + 0.6 * np.sin(2 * np.pi * freq * np.arange(n) / SR)
    cut = 320 + 1100 * np.exp(-np.arange(n) / SR / 0.06)
    s = onepole_lp(s, cut)
    e = env(n, 0.003, 0.5)
    e[-200:] *= np.linspace(1, 0, 200)
    return np.tanh(s * 2.2) * e * 0.55


def pluck(freq, length=0.22):
    n = int(length * SR)
    s = saw(freq, n, 0.006)
    cut = 600 + 5000 * np.exp(-np.arange(n) / SR / 0.05)
    return onepole_lp(s, cut) * env(n, 0.001, 0.09) * 0.22


def pad(freqs, length):
    n = int(length * SR)
    s = sum(saw(f, n, 0.008) for f in freqs) / len(freqs)
    s = onepole_lp(s, 1800)
    t = np.arange(n) / SR
    e = np.clip(t / 0.02, 0, 1) * np.exp(-t / 2.2)
    return s * e * 0.5


def riser(length, gain=0.35):
    n = int(length * SR)
    t = np.arange(n) / SR
    noise = rng.standard_normal(n)
    cut = 300 + 9000 * (t / length) ** 2
    s = noise - onepole_lp(noise, cut)
    sweep = np.sin(2 * np.pi * np.cumsum(200 + 1400 * (t / length) ** 2) / SR) * 0.25
    return (s * 0.6 + sweep) * (t / length) ** 2 * gain


def impact():
    n = int(2.0 * SR)
    t = np.arange(n) / SR
    f = 38 + 80 * np.exp(-t / 0.08)
    boom = np.sin(2 * np.pi * np.cumsum(f) / SR) * np.exp(-t / 0.7)
    noise = rng.standard_normal(n)
    crash = (noise - onepole_lp(noise, 3000)) * np.exp(-t / 0.5) * 0.35
    return np.tanh(boom * 1.4) * 0.9 + crash


def stab(freqs):
    n = int(0.3 * SR)
    s = sum(saw(f, n, 0.01) for f in freqs) / len(freqs)
    return onepole_lp(s, 2500) * env(n, 0.001, 0.08) * 0.6


def mtof(m):
    return 440.0 * 2 ** ((m - 69) / 12)


def reed(freq, length, gain=0.3):
    """Clarinet-ish: odd harmonics, soft attack, light vibrato."""
    n = int(length * SR)
    t = np.arange(n) / SR
    vib = 1 + 0.004 * np.sin(2 * np.pi * 5.5 * t) * np.clip(t / 0.12, 0, 1)
    ph = 2 * np.pi * np.cumsum(freq * vib) / SR
    s = sum(np.sin(k * ph) / k ** 1.3 for k in (1, 3, 5, 7, 9))
    s += 0.12 * np.sin(2 * ph)
    e = np.clip(t / 0.012, 0, 1) * np.exp(-t / 0.35)
    e[-300:] *= np.linspace(1, 0, 300)
    return np.tanh(s * 1.2) * e * gain


# D minor with a raised leading tone (C#) and Ukrainian-Dorian G#, 137 BPM,
# the harmonic language of the reference track.
D2 = mtof(38)
BASS_ROOTS = [38, 38, 45, 37]          # D D A C#  (bar 4 resolves to D on beat 3)
RIFF_A = [(62, 62), (64, 65), (65, 62), (61, 62)]   # D-D  E-F  F-D  C#-D
RIFF_B = [(62, 62), (64, 65), (68, 69), (61, 62)]   # ... G#-A lift
CHORDS = [
    [62, 65, 69],   # Dm
    [62, 65, 69],   # Dm
    [61, 64, 69],   # A
    [61, 65, 69],   # A(b9-ish) -> Dm
]

# --- Bar 0: hook -----------------------------------------------------------
for b in range(3):
    add(kick(0.9), b * BEAT)
    add(stab([mtof(50), mtof(53), mtof(57)]), b * BEAT, 0.8)
add(riser(BEAT * 1.0, 0.45), BEAT * 3)

# --- Bars 1-13: groove -----------------------------------------------------
add(impact(), BAR * 1, 0.8)
for bar in range(1, 14):
    t0 = bar * BAR
    ci = (bar - 1) % 4
    blitz = bar >= 12
    for beat in range(4):
        t = t0 + beat * BEAT
        add(kick(), t)
        if beat in (1, 3):
            add(clap(), t, 0.8)
        root = BASS_ROOTS[ci]
        if ci == 3 and beat >= 2:
            root = 38
        add(bass_note(mtof(root), BEAT / 2 * 0.95), t + BEAT / 2, 1.0)
        hats = 4 if blitz else 2
        for h in range(hats):
            if hats == 2 and h == 0:
                continue
            add(hat(0.03 if h % 2 else 0.02), t + h * BEAT / hats, 0.55,
                pan=0.3 if h % 2 else -0.3)
    if bar >= 2:
        riff = RIFF_B if ((bar - 2) // 4) % 2 else RIFF_A
        a, b = riff[ci]
        for pos, note in ((1, a), (2, a), (5, b), (6, b)):
            add(reed(mtof(note), BEAT / 2 * 0.9, 0.75), t0 + pos * BEAT / 2, pan=0.1)
        # quiet off-grid chord plucks fill the gaps the riff leaves
        for pos in (0, 3, 4, 7):
            for m in CHORDS[ci]:
                add(pluck(mtof(m + 12), 0.15), t0 + pos * BEAT / 2, 0.8, pan=-0.25)
    if bar == 13:
        for s16 in range(16):
            add(clap(), t0 + s16 * BEAT / 4, 0.2 + 0.5 * s16 / 16)
        add(riser(BAR, 0.5), t0)

for bar in range(2, 12):
    add(riser(BEAT * 0.5, 0.1), bar * BAR - BEAT * 0.5)

# --- Bars 14+: end card -----------------------------------------------------
end = 14 * BAR
add(impact(), end, 1.0)
add(pad([mtof(m) for m in (50, 53, 57, 62)], DUR - end), end, 0.55)
add(pad([mtof(38), mtof(45)], DUR - end), end, 0.45)
for beat in range(4):
    add(kick(0.8), end + beat * BEAT * 2)
for i, note in enumerate([62, 64, 65, 68, 69, 68, 65, 64, 62, 61, 62]):
    add(reed(mtof(note + 12), BEAT / 2 * 0.95, 0.22 * (1 - i / 14)), end + i * BEAT / 2)

mix = np.stack([L, R], axis=1)
fade = np.ones(N)
fade[-int(1.2 * SR):] = np.linspace(1, 0, int(1.2 * SR)) ** 2
mix *= fade[:, None]
mix = np.tanh(mix * 0.9)
mix /= np.max(np.abs(mix)) / 0.89

out = sys.argv[1] if len(sys.argv) > 1 else "music.wav"
with wave.open(out, "wb") as w:
    w.setnchannels(2)
    w.setsampwidth(2)
    w.setframerate(SR)
    w.writeframes((mix * 32767).astype("<i2").tobytes())
print("wrote", out)
