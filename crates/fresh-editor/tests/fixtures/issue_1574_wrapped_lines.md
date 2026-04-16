# Wrapped Buffer Scroll Test

This file contains many long paragraphs separated by blank lines. Each paragraph is deliberately long enough that it wraps across several visual rows in an 80-column terminal, so the viewport's wrap math is exercised while the cursor walks from the top of the document down to the end.

Paragraph one: the quick brown fox jumps over the lazy dog, a pangram sentence that contains every letter of the English alphabet at least once. This sentence is repeated here with minor variations so the whole paragraph wraps to several visual lines: the quick brown fox jumps over the lazy dog, again and again, until the line is long enough to wrap a few times in the editor viewport used by the regression test.

Paragraph two: Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.

Paragraph three: Sphinx of black quartz, judge my vow. Pack my box with five dozen liquor jugs. How vexingly quick daft zebras jump. Waltz, bad nymph, for quick jigs vex. The five boxing wizards jump quickly. This paragraph is intentionally verbose so that even in a reasonably wide terminal window it still wraps over several visual rows of the editor viewport.

Paragraph four: All work and no play makes Jack a dull boy, and so we repeat this sentence at length to ensure that the paragraph is long enough to wrap several times: all work and no play makes Jack a dull boy; all work and no play makes Jack a dull boy; all work and no play makes Jack a dull boy; all work and no play makes Jack a dull boy.

Paragraph five: A long line intended to exceed the width of the terminal so that it wraps across many visual rows when line wrapping is enabled in the editor configuration, exercising the visual-row-aware scroll math in the viewport module. This paragraph is padded further with additional filler text so that even at generous widths the line still spans several visual rows.

Paragraph six: One fish two fish red fish blue fish, green eggs and ham, the cat in the hat, hop on pop, fox in socks, oh the places you will go — classic lines strung together to make a paragraph that wraps across many visual lines in the editor viewport, exercising the scroll math for heavily wrapped content as the cursor moves downward.

Paragraph seven: It was the best of times, it was the worst of times, it was the age of wisdom, it was the age of foolishness, it was the epoch of belief, it was the epoch of incredulity, it was the season of light, it was the season of darkness, it was the spring of hope, it was the winter of despair.

Paragraph eight: Call me Ishmael. Some years ago — never mind how long precisely — having little or no money in my purse, and nothing particular to interest me on shore, I thought I would sail about a little and see the watery part of the world. It is a way I have of driving off the spleen and regulating the circulation.

Paragraph nine: Happy families are all alike; every unhappy family is unhappy in its own way. Everything was in confusion in the Oblonskys' house. The wife had discovered that the husband was carrying on an intrigue with a French girl who had been a governess in their family, and she had announced to her husband that she could not go on living in the same house with him.

Paragraph ten: In a hole in the ground there lived a hobbit. Not a nasty, dirty, wet hole, filled with the ends of worms and an oozy smell, nor yet a dry, bare, sandy hole with nothing in it to sit down on or to eat: it was a hobbit-hole, and that means comfort. This was a perfectly ordinary Bag End hobbit-hole and it wrapped across many visual rows.

Paragraph eleven: Mr. and Mrs. Dursley, of number four, Privet Drive, were proud to say that they were perfectly normal, thank you very much. They were the last people you would expect to be involved in anything strange or mysterious, because they just did not hold with such nonsense. This paragraph also wraps to many visual rows.

Paragraph twelve: It is a truth universally acknowledged, that a single man in possession of a good fortune, must be in want of a wife. However little known the feelings or views of such a man may be on his first entering a neighbourhood, this truth is so well fixed in the minds of the surrounding families that he is considered rightful property of some one or other of their daughters.

End of the wrapped-buffer scroll fixture.
