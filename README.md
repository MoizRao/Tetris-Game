# Tetris Game

A classic Tetris-style game implemented in x86 Assembly language. This project demonstrates low-level programming concepts while creating an engaging gaming experience.

## 🎮 Game Features

- Classic tetromino shapes (L, T, U, I, Box, line shapes)
- Score tracking system
- Next shape preview
- Game timer
- Collision detection
- Line clearing mechanics
- Pause functionality
- Game over detection

## 🛠️ Prerequisites

To run this game, you'll need:

1. NASM (Netwide Assembler)
   - Required for assembling the source code
   - Version 2.0 or higher recommended

2. DOSBox
   - Required for emulating the DOS environment
   - Version 0.74 or higher recommended

## 🚀 Installation & Running

1. Clone or download the source code
2. Open DOSBox
3. Mount your project directory:
   ```
   mount c c:\path\to\project
   c:
   ```
4. Assemble the code using NASM:
   ```
   nasm tetris.asm -o tetris.com
   ```
5. Run the game:
   ```
   tetris.com
   ```

## 🎯 Game Controls

- **←** (Left Arrow): Move shape left
- **→** (Right Arrow): Move shape right
- **↓** (Down Arrow): Accelerate shape descent
- **P**: Pause game
  - Press Enter to resume
- **ESC**: Exit game

## 🎲 Gameplay Elements

### Scoring System
- 50 points awarded for each line cleared
- Score displayed in real-time

### Timer
- Game tracks playing time
- Display format: MM:SS
- Game ends after 5 minutes

### Next Shape Preview
- Shows upcoming shape in the side panel
- Helps with strategic planning

## 🎨 Display Layout

- Main game area on the left
- Side panel showing:
  - "GOOD LUCK!" message
  - Current score
  - Time elapsed
  - Next shape preview
- Promotional message: "Powered By Brox Gaming"

## 🔧 Technical Details

- Written in x86 Assembly language
- Uses BIOS and DOS interrupts for:
  - Keyboard input handling
  - Screen output
  - Timer functionality
- Video memory manipulation for graphics
- Implements custom interrupt handlers
- Real-time game loop implementation

## ⚠️ Notes

- Game requires a DOS environment or emulator
- Designed for 80x25 text mode display
- Uses text characters for graphics
- Color attributes used for visual enhancement

## 🐛 Known Limitations

- Fixed game duration (5 minutes)
- No shape rotation functionality
- Text-based graphics
- DOS platform dependent

## 🤝 Contributing

Feel free to fork this project and submit improvements through pull requests. Some areas for potential enhancement:

- Shape rotation implementation
- High score system
- Sound effects
- Different difficulty levels
- Custom game duration options

## 📝 License

This project is open source and available for educational and personal use.
