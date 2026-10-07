# Asking the user for input

There are seven ways a plugin can ask the user for input. Each example adds an `Ask: …` command to the command palette. Paste one into `~/.config/fresh/init.ts` to try it.

## 1. Prompt line

Asks for text on the bottom line. This takes the least code.

```ts
const value = await editor.prompt("Issue number:", "");
```

[Example code](https://github.com/sinelaw/fresh/blob/master/docs/plugins/examples/asking-for-input/1_prompt_line.ts) · API: [`prompt`](../../api/buffer#prompt)

![Prompt line](./screenshots/1.png)

## 2. Floating prompt

Asks for text in a box in the middle of the screen, with a list under it. The list can update as the user types.

[Example code](https://github.com/sinelaw/fresh/blob/master/docs/plugins/examples/asking-for-input/2_floating_prompt.ts) · API: [`startPrompt`](../../api/buffer#startprompt), [`setPromptSuggestions`](../../api/buffer#setpromptsuggestions), [`setPromptFooter`](../../api/buffer#setpromptfooter), [prompt events](../../api/events#prompt-popup-and-dialog-events)

![Floating prompt](./screenshots/2.png)

## 3. Pick list

Shows a list of choices on the bottom line. The user picks one.

[Example code](https://github.com/sinelaw/fresh/blob/master/docs/plugins/examples/asking-for-input/3_pick_list.ts) · API: [`startPrompt`](../../api/buffer#startprompt), [`setPromptSuggestions`](../../api/buffer#setpromptsuggestions), [prompt events](../../api/events#prompt-popup-and-dialog-events)

![Pick list](./screenshots/3.png)

## 4. Dialog

Shows a dialog box with fields, checkboxes and buttons. Use it to ask for several things at once.

[Example code](https://github.com/sinelaw/fresh/blob/master/docs/plugins/examples/asking-for-input/4_modal_dialog.ts) · API: [`mountFloatingWidget`](../../api/buffer#mountfloatingwidget), [`updateFloatingWidget`](../../api/buffer#updatefloatingwidget), [`unmountFloatingWidget`](../../api/buffer#unmountfloatingwidget), [`widget_event`](../../api/events#prompt-popup-and-dialog-events)

![Dialog](./screenshots/4.png)

## 5. Popup

Shows a short message with a few choices.

[Example code](https://github.com/sinelaw/fresh/blob/master/docs/plugins/examples/asking-for-input/5_action_popup.ts) · API: [`showActionPopup`](../../api/buffer#showactionpopup), [`action_popup_result`](../../api/events#prompt-popup-and-dialog-events)

![Popup](./screenshots/5.png)

## 6. Single key

Waits for the user to press one key. The user doesn't press Enter.

[Example code](https://github.com/sinelaw/fresh/blob/master/docs/plugins/examples/asking-for-input/6_single_key.ts) · API: [`getNextKey`](../../api/buffer#getnextkey)

![Single key](./screenshots/6.png)

## 7. File picker

Lets the user choose a file. Returns the file's path.

[Example code](https://github.com/sinelaw/fresh/blob/master/docs/plugins/examples/asking-for-input/7_file_picker.ts) · API: [`pickFile`](../../api/buffer#pickfile)

![File picker](./screenshots/7.png)
