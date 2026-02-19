# Letter Soup

Letter Soup is a Scala-based word search puzzle game with two interfaces TUI and GUI. Players find hidden words in a letter grid before time expires. Game mechanics include selecting adjacent letters in 8 directions. 

## Project Structure
- **UtilsGameEngine.scala**: Core logic of the game engine.
- **TUIGameEngine.scala**: Text interface for TUI gameplay.
- **GUIGameEngine.scala**: Graphical interface using JavaFX.
- **GUIGameEngineController.scala**: Controller for the graphical interface.
- **Level Maker Tool**: For creating custom boards.
- **Utilities**: For directions, file management, and general operations.

## Features
- Interactive gameplay with timers.
- Two UI options (TUI and GUI).
- Custom level creation.
- Persistent storage for saved games.
- Visual feedback with color highlighting.
- Support for multiple board sizes.

Players can click letters to trace words or use text commands.

## Technologies
- **Language**: Scala
- **Framework**: JavaFX
