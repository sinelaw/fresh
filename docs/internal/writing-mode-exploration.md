# Writing Mode: A User-First Exploration

> _AI-generated exploration. Entirely **PLANNED**: nothing here ships. It
> starts from the people and works inward, and touches the implementation
> only at the end, to say what Fresh already has and where the gaps are._

Purpose: work out what a **word-processor-like writing mode** for Fresh would
need to be for one specific audience: people who are privacy-aware, wary of AI
and local-first, and who want a simple but modern place to write journals,
novels, essays and letters, and to **share what they write without leaking
it**. The question is not "what can Fresh render?" but "what does this person
need, and what should we refuse to build?"

---

## 1. Who this is for

Three overlapping people. They share one stance: *my words are mine, they
live on my machine, and nothing touches them that I didn't invite.*

| | The journaler | The novelist | The essayist / letter writer |
|---|---|---|---|
| Writes | Daily, short, intensely private | Long sessions, 80–120k words over months | Occasional pieces, a few thousand words |
| Biggest fear | Someone reading it: a partner, an employer, a breach, a vendor's AI | Losing it: corruption, sync accidents. Or someone taking it | Sending something that reveals more than intended |
| Needs from structure | Entries by date, search, "today" | Chapters/scenes, ordering, a word-count goal, one compiled manuscript | One document, maybe footnotes |
| Shares | Almost never; maybe excerpts with a therapist or family | With beta readers, then with agents and editors (as DOCX) | As PDF, HTML or email text |
| Formatting tolerance | Nearly none | Italics, scene breaks, chapter headings | Headings, lists, quotes, links |

What they have in common:

- **They are not necessarily terminal users.** Many are refugees from Word,
  Google Docs, Scrivener, Day One or Ulysses. A terminal-only writing mode
  serves a slice of them (the people who today write prose in Vim with
  Goyo/Pencil, Emacs with Olivetti, or WordGrinder). The rest need the
  windowed build. That matters more for this audience than for any other
  Fresh feature (see §7, Q1).
- **They read terms of service now.** The audience has been primed by
  AI-training controversies. Trust comes from what the tool *structurally
  cannot do*, not from a privacy policy.
- **They want fewer features, not more.** People praise WordGrinder for having
  "everything needed and no more", and the feature lists of iA Writer and
  FocusWriter are short on purpose. Scrivener's power comes with a reputation
  for a steep learning curve.

## 2. What they're leaving, and why

These push factors are recurring themes in the material reviewed (sources at
the end). Most of the sources are blogs, forums and vendor pages, so they
show sentiment rather than measuring it.

1. **AI showing up inside the writing surface.** Gemini in Google Docs is on
   by default and easy to trigger by accident. Copilot in Microsoft 365 came
   with a price rise, and the opt-outs were hard to find; at one point
   Microsoft's own assistant told a reporter that "Draft with Copilot" could
   not be removed. People who switched to LibreOffice describe Copilot as
   "the last straw". The fear is twofold: being *interrupted* by AI, and
   having their words *used* by it.
2. **Cloud custody.** Even writers who don't fear training dislike having
   their only copy on someone else's server. The local-first ideals (Ink &
   Switch: fast, multi-device, offline, collaboration, longevity, privacy,
   user control) are the vocabulary this audience reaches for, and
   **longevity** (files that outlive the vendor) comes up again and again.
3. **"Local" that isn't private.** Obsidian keeps plain Markdown on disk,
   which this audience likes, but the files are unencrypted at rest, and the
   app is closed source. Journalers in particular want encryption at rest,
   not just "not in the cloud".
4. **Sync that eats work.** The Scrivener forums are full of scenes and
   chapters emptied by Dropbox sync, including backups that synced the
   corruption too. The lesson writers take from it: *sync is not backup*,
   and a project format made of many interdependent files is fragile.
5. **Files that betray them when shared.** A .docx carries the author's name,
   company, editing time and revision count, plus tracked changes and
   comments that can survive "Accept All". There is a real threat, too:
   Filippo Bernardini phished more than a thousand unpublished manuscripts
   out of authors and publishers using look-alike domains. Writers already
   follow folk rules: send each reader their own copy, never the master,
   watermark ARCs, prefer EPUB/PDF to an editable file.
6. **Proving it's theirs.** The Authors Guild's "Human Authored"
   certification (2025) runs on an honor code, because AI detection doesn't
   work reliably. iA Writer now tracks typed versus pasted text. For an
   AI-averse writer, being able to show where the words came from is
   becoming a feature in its own right.

## 3. What they use today, and the gap

| Tool | What they love | Where it falls short for this audience |
|---|---|---|
| iA Writer | Focus mode, typewriter scrolling, on-device style check, authorship tracking | Not open source, mostly Apple-centric, no encryption at rest |
| Scrivener | Binder, snapshots, compile to manuscript | Steep learning curve, fragile under sync, proprietary project format |
| Ulysses / Day One | Polish, journaling features (templates, lock, search) | Subscription and cloud; Day One's App Store listing declares collected data |
| Obsidian (+ Longform + Pandoc) | Plain files, local, compile scenes then export to DOCX | Unencrypted at rest, closed source, a plugin stack the writer has to assemble |
| novelWriter | Open source, plain text, project notes, comments excluded from the word count | Python/Qt app, ODT/HTML export rather than DOCX |
| FocusWriter | Free, goals, timers, distraction-free | Little structure, no encryption |
| WordGrinder | Terminal, "everything needed and no more" | Barely any styles, binary native format |
| jrnl | CLI journal, plain text, optional AES | Not an editor; you write entries in `$EDITOR` |
| Mini Diarium | Open source, AES-256-GCM, never online | A journal only; no long-form writing |
| LibreOffice | Familiar, reads/writes DOCX | A page-layout tool: exactly the knobs this audience wants hidden |

**The gap:** no single open-source tool offers a **calm writing surface**,
**plain files you own**, **optional strong encryption at rest**, **local-only
proofing**, and **export that is safe to share**, for both the diary and the
novel. Today people assemble it from three or four tools (Obsidian +
Longform + Pandoc + an encrypted disk).

## 4. Promises

These are product promises. Each one should be checkable by a skeptical user,
not just stated.

1. **Nothing leaves this machine.** Writing mode makes zero network requests:
   no update check, no telemetry, no plugin fetches. It says so in one line,
   and the user can verify it.
2. **No AI. Not hidden, not optional, not "coming soon".** There is no
   generative feature to switch off, because none exists. Proofing is
   dictionaries and rules that run locally.
3. **Your words are ordinary files.** Markdown (or something just as plain)
   in a folder you choose. They open in any text editor in 30 years, and if
   Fresh disappears, nothing is lost.
4. **Private means encrypted, including the leftovers.** An encrypted
   document never touches disk as plaintext: no recovery file, no hot-exit
   copy, no search index, no clipboard history.
5. **Sharing is a deliberate act, and you see exactly what goes out.** Exports
   carry only the text the writer chose, with no hidden metadata, notes or
   history.
6. **Writing first, formatting second.** Formatting is a small, closed set,
   and the page always looks like prose, never like markup.

## 5. The minimal feature set

Scored against the promises and the people above. **Must** is the smallest
thing worth calling a writing mode. **Should** follows soon after. **Maybe**
needs more thought. **No** is a deliberate refusal.

### 5.1 The writing surface

| | Feature | Why |
|---|---|---|
| Must | **Calm page**: centered text column at a readable width, no line numbers, gutter, tabs, minimap, diagnostics or status noise | The core ask of every distraction-free tool |
| Must | **Concealed markup**: `*italic*` shows as italic; the marks appear only on the line being edited | Plain files underneath, prose on screen |
| Must | **Typewriter scrolling**: the current line stays at a fixed height | Widely loved in iA Writer; reduces neck-craning on long sessions |
| Must | **Smart typography as you type**: curly quotes, apostrophes, em/en dash from `--`/`---`, ellipsis, all reversible with one undo | Novelists expect it; straight quotes are a tell of a code editor |
| Must | **Word count**: document, selection and *this session*, shown quietly and hideable | Universal; the session count is what keeps people going |
| Should | **Focus dimming**: everything but the current sentence or paragraph fades | iA's signature; some writers love it, so it's off by default |
| Should | **Goals**: a target for the session or the project, with a quiet progress indicator | FocusWriter, Scrivener, NaNoWriMo culture |
| Should | **A prose font and line spacing** (windowed build) | Monospace is fine for some, alienating for most of this audience |
| Maybe | Sprint timer | Popular but borderline gamified; a plugin rather than core |
| No | Fonts, colors, sizes, page layout, margins-as-formatting | That's LibreOffice, and they're leaving it |

### 5.2 Formatting (the closed set)

Just enough for a novel manuscript, a journal and an essay:

- **Headings**: three levels (book/part, chapter, section). novelWriter
  deliberately stops at four; three is enough here.
- **Emphasis**: italic, bold. (Underline is only a manuscript convention for
  italics; the exporter can do that mapping, the writer shouldn't.)
- **Block quote**, **bulleted and numbered lists**.
- **Scene break** as a first-class element (shown as a centered `* * *`,
  exported correctly for each format).
- **Links** (essays) and **footnotes** (essays, some non-fiction).
- **Writer's notes**: comments that are visible while writing but **never**
  exported or counted. novelWriter's `%` comments are the precedent.
  The safe-sharing promise depends on them being excluded by construction.

Not in the set: tables, images, colors, alignment, custom styles. Anything the
writer types outside the set (raw HTML, say) stays as plain text: it is shown,
and it is not interpreted.

### 5.3 Organizing

| | Feature | For |
|---|---|---|
| Must | **Open a folder as a writing project**: files are chapters, entries or pieces | Everyone |
| Must | **Outline sidebar** (headings of the current file) and **file list** in an explicit order | Novelist |
| Must | **Journal: "Today"** opens or creates the dated entry (`2026/10/25.md`), with a short list of recent entries | Journaler |
| Must | **Search across the project**, done locally and, for encrypted projects, in memory | Everyone |
| Should | **Reorder chapters/scenes** by dragging or with keys; order kept in a small, human-readable file | Novelist |
| Should | **Snapshots**: "save a version" of a chapter before a big rewrite, browse and restore, stored beside the project in plain files (encrypted when the project is) | Scrivener's best-loved feature, without its fragility |
| Should | **Backups to a second place** (a USB drive, another folder): one command, clearly *not* sync | The Scrivener lesson |
| Maybe | Journal calendar view, tags | Day One parity, but adds surface |
| No | Cloud sync, accounts, real-time collaboration | Users bring their own sync (Syncthing, a USB stick), and ideally sync only ciphertext |

### 5.4 Privacy and safety

| | Feature | Notes |
|---|---|---|
| Must | **Offline by construction** in writing mode (see promise 1) | Fresh's update check is on by default today; writing mode must turn it off, not merely let the user do so |
| Must | **Encrypted documents/projects with a passphrase**, in an existing, documented open format (age's passphrase mode is a strong candidate) so files can be decrypted *without Fresh* | Longevity: if you need Fresh to read your diary, the promise is broken |
| Must | **No plaintext residue**: recovery/hot-exit data for encrypted buffers is encrypted or not written at all, and the user is told which | The single most important thing to get right; see §8 |
| Must | **Lock**: idle timeout or a command that clears the decrypted text from memory and the screen | Journalers on shared computers |
| Must | **Honest passphrase UX**: "If you forget this, nobody can recover your writing, including us." Optional recovery key the user can print | The usual failure mode of encrypted journals |
| Should | **Clipboard hygiene** for encrypted docs (warn, or clear after a timeout) | Defense in depth |
| No | Key escrow, "forgot password" via a server | Contradicts the promises |

### 5.5 Proofing (local only)

| | Feature | Notes |
|---|---|---|
| Must | **Spell check** with standard Hunspell-format dictionaries, a personal word list stored in the project (character names), and multiple languages | Universally expected; entirely offline |
| Should | **Style checks as rules, not AI**: fillers, clichés, repeated words, adverbs, passive voice, user-defined patterns | iA's Style Check is on-device and loved; an offline rule-based grammar checker (e.g. Harper, which runs locally) could plug in via Fresh's universal-LSP slot |
| No | Rewrite suggestions, autocomplete-as-you-type, "continue writing", summaries | Promise 2 |

### 5.6 Sharing safely

This is where Fresh can do something the incumbents don't. Sharing is a
**separate, explicit flow**: *Share…* shows a preview of exactly what the
recipient will get.

| | Feature | Notes |
|---|---|---|
| Must | **Export to DOCX in standard manuscript format** (Shunn-style: contact block, word count, running header, double spacing, chapter starts partway down the page) | Agents and editors ask for .doc/.docx; this is the one-click thing novelists will judge the mode on |
| Must | **Export to PDF, EPUB, HTML and plain text** | Beta readers, family, the web |
| Must | **Metadata-clean by default**: no author or company unless typed in, no edit time, no revision count, no tracked changes, no writer's notes, no snapshots | The .docx leaks of §2 are exactly what this prevents |
| Must | **Export a selection or range** ("chapters 1–3", "this entry") | Journal excerpts; agent sample pages |
| Should | **Per-recipient copies**: each export is labeled with a recipient name, carries a visible footer and/or an invisible marker, and gets a local "sent log" (who got what version, when) | Encodes writers' existing folk practice: "send each reader their own copy", "trace the leak" |
| Should | **Encrypted share**: export to an age-encrypted file for a recipient's passphrase or public key | For the trusted reader; nothing in the middle can read it |
| Should | **Copy as clean text for email**: agents often want the first pages in the email body | Smart quotes kept, markup gone |
| Maybe | **Authorship record**: optional, local, per-project log of typed versus pasted text, exportable as a statement to support a "human authored" claim | Valuable to this audience, but it is self-surveillance: strictly opt-in, local, and never on by default |
| No | Share links, hosting, "publish to web" | Any server in the loop breaks promise 1 |

## 6. Journeys

Short walkthroughs to test the feature set against.

**First run.** The writer opens Fresh's writing mode (a launcher entry, or
`fresh --write ~/Writing`). One quiet screen: *"Writing mode. Offline, no
AI. Your files live in ~/Writing."* Two choices: **Start a journal** /
**Start a project**, plus "Encrypt this?" with the passphrase warning. Then
a blank page. No tour.

**A journal morning.** "Today" opens a fresh dated entry, or yesterday's if
it's 2am (a configurable day boundary). The writer types; the word count for
the session is the only number visible. They close the laptop. On reopen the
entry is locked; the passphrase restores it.

**A novel session.** The writer opens the project to the last chapter and
position. Typewriter scrolling is on, and the session goal sits at 400/1000.
They take a snapshot ("before cutting the flashback"), then cut 2,000 words.
The outline updates. Notes to self (`% check the timeline`) stay visible but
are excluded from the count.

**Querying an agent.** *Share… → Manuscript (DOCX)*, chapters 1–3. The
preview shows the contact block and word count; the "What's included" list
says *no notes, no metadata, no history*. The recipient label is
"Agent: Jane Doe". The file is saved, and the sent log records it. Then
*Copy as clean text*, first five pages, into the query email.

**Beta readers.** One EPUB per reader, each labeled and logged. When a copy
turns up somewhere it shouldn't, the marker says whose it was.

**The lost laptop.** The disk is unencrypted and the journal folder is
copied by whoever finds it. They get age files and nothing else: no
plaintext recovery files, no search index, no thumbnails.

## 7. Open questions (decisions for the maintainers)

1. **Terminal or window first?** The terminal build gives the purest
   distraction-free experience, but this audience is mostly *not* terminal
   users. A writing mode that only works in a terminal is a niche within a
   niche. Recommendation: design for both, but measure success on the
   windowed build (prose font, ordinary file dialogs, an icon to launch).
2. **Markdown as storage, or a stricter subset?** Markdown is plain and
   portable, but writers will paste things (raw HTML, tables) and expect
   them to "work". Recommendation: store CommonMark, plus footnotes and a
   scene-break convention. Treat anything outside the §5.2 set as plain
   text: shown, not interpreted.
3. **Encryption granularity.** Per file (simple, each entry stands alone) or
   per project (one passphrase, encrypted metadata and file names)? Journalers
   care that even entry *titles and dates* are private, which argues for
   per-project encryption with encrypted names.
4. **A mode or a separate product face?** A "writing profile" that hides the
   IDE (LSP, git, terminals, agents) is cheap, but a writer who stumbles into
   the code-editor UI will feel lost. Perhaps writing mode is its own launch
   target with its own (tiny) menus.
5. **Plugins in writing mode.** Plugins are Fresh's strength, but promise 1
   ("nothing leaves") is only as strong as the least careful plugin.
   Options: writing mode runs only bundled, audited plugins; or plugins are
   allowed with a visible "network: none" guarantee from the host.
6. **How visible should the "no AI" stance be?** Stating it plainly on the
   first screen is what this audience wants to hear. It is also a lasting
   commitment that the project should make knowingly.

## 8. Where Fresh stands today (brief)

Only the implementation context needed to judge feasibility:

- **Already there:** page view / compose mode (centered column at a
  configurable width, concealed markup, soft-break re-flow, rendered tables),
  source/rendered/dual views, a Contents sidebar for headings, smart Markdown
  list editing, hot exit and crash recovery, per-language settings
  (`page_view`, `page_width`, `line_wrap`), a windowed (GUI) build, and a
  universal-LSP slot suited to an offline spell/grammar server.
- **Missing:** spell check, word counts and goals, typewriter scrolling,
  focus dimming, smart typography, snapshots, encryption, export (DOCX
  manuscript, EPUB, PDF), share flow, a writing-first launch surface.
- **A conflict to resolve first:** crash recovery writes raw buffer contents
  to the user data directory every couple of seconds, and hot exit persists
  buffers across sessions by default. That is right for code and exactly
  wrong for an encrypted journal (promise 4). Encrypted buffers need a
  recovery path that writes only ciphertext, or none, before encryption can
  ship.
- **Also:** the update check and anonymous telemetry are on by default
  (see `docs/privacy.md`). Writing mode has to be offline without the user
  having to go looking for a setting.

## 9. A first slice

The smallest release that would make this audience take notice:

1. A **writing profile**: calm page, concealed markup, prose font (GUI),
   typewriter scrolling, smart typography, word count with session count,
   network off.
2. **Offline spell check** with a project word list.
3. **Clean export**: DOCX manuscript format, PDF and plain text, with
   metadata stripped and writer's notes excluded.
4. **Journal "Today"**.

Encryption (with the residue problem fixed), snapshots and the share flow
come next. They are what makes this mode *different*; the first slice
earns the right to build them.

---

## Sources

User-side research (October 2026). Most sources are blogs, forums, vendor
pages or press; they indicate sentiment and practice, not measured
prevalence.

- AI in writing tools: [Google Docs and Gemini AI privacy (Fileverse)](https://blog.fileverse.io/google-docs-gemini-ai-privacy-2025/), [Best Google Docs alternatives for writers (CipherWrite)](https://cipherwrite.com/blog/best-google-docs-alternatives-for-writers), [Copilot in Microsoft 365 Personal/Family (The Register)](https://www.theregister.com/2025/01/17/copilot_microsoft_365/), [Turning off Copilot in Word (Microsoft Answers)](https://answers.microsoft.com/en-us/msoffice/forum/all/how-to-turn-off-copilot-in-word-for-windows/b51e0127-4e54-4619-a37b-08f8de0616ac), [Killing my Copilot](https://quirkworthy.com/2026/03/12/killing-my-copilot/), [Editors without AI (Latenode community)](https://community.latenode.com/t/looking-for-document-editors-without-ai-features-instead-of-google-docs/37690)
- Local-first and privacy: [Local-first software (Wikipedia)](https://en.wikipedia.org/wiki/Local-first_software), [Obsidian (Reclaim The Net)](https://reclaimthenet.org/recommended-tool/obsidian), [Mini Diarium (Flathub)](https://flathub.org/zh-Hant/apps/io.github.fjrevoredo.mini-diarium), [Privacy Notes (It's FOSS)](https://itsfoss.com/privacy-notes/), [jrnl overview](https://jrnl.sh/en/stable/overview), [age (Linux Magazine)](https://www.linux-magazine.com/Issues/2023/266/Coming-of-Age)
- Writing tools: [iA Writer](https://ia.net/writer), [Ulysses vs Scrivener (Reedsy)](https://reedsy.com/studio/resources/ulysses-vs-scrivener/), [WordGrinder (LinuxInsider)](https://www.linuxinsider.com/?p=88211), [WordGrinder (AlternativeTo)](https://alternativeto.net/software/wordgrinder/about), [novelWriter (LinuxLinks)](https://www.linuxlinks.com/novelwriter-plain-text-editor-novels/), [Obsidian Longform](https://community.obsidian.md/plugins/longform), [Write novels in plain text](https://pdworkman.com/write-novels-in-plain-text), [My writing workflow (Jon Reeve)](https://jonreeve.com/2025/06/my-writing-workflow), [Markdown to manuscript format with Pandoc](https://www.autodidacts.io/convert-markdown-to-standard-manuscript-format-odts-docs-and-pdfs-with-pandoc.md), [Day One (AlternativeTo)](https://www.alternativeto.net/software/day-one/about/)
- Data loss: [Scrivener losing chapters during Dropbox sync](https://forum.literatureandlatte.com/t/scrivener-losing-chapters-during-dropbox-sync/122779), [Empty pages](https://forum.literatureandlatte.com/t/empty-pages/123867)
- Sharing and leaks: [Hidden metadata in documents (Platoforms)](https://www.platoforms.com/blog/hidden-metadata-documents/), [Word hidden data (Marquette ITS)](https://marquette.edu/its/help/security/wordhidden.shtml), [Manuscript phishing guilty plea (Smithsonian)](https://www.smithsonianmag.com/smart-news/from-margaret-atwood-to-sally-rooney-he-tried-to-steal-over-1000-unpublished-manuscripts-180981431/), [Arrest (The Record)](https://therecord.media/italian-man-arrested-for-stealing-unpublished-book-manuscripts/), [Share a manuscript safely (Just Publishing Advice)](https://justpublishingadvice.com/share-a-book-manuscript-file/), [ARC piracy prevention (Indie Author Magazine)](https://indieauthormagazine.com/piracy-prevention-programs-for-your-arcs-maiden-voyage)
- Manuscript format: [Standard manuscript format (Wikipedia)](https://en.wikipedia.org/wiki/Standard_manuscript_format), [Manuscript format guide (Kindlepreneur)](https://kindlepreneur.com/manuscript-format/)
- Human authorship: [Authors Guild Human Authored certification](https://www.authorsguild.org/industry-advocacy/human-authored-certification/), [Marketplace on the label](https://www.marketplace.org/2024/10/07/authors-guild-human-authored-label-ai)
