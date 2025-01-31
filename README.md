# CPU Frequency Monitor

CPU Frequency Monitor is an open-source Linux GUI application that displays the frequency and utilization of each CPU core in real-time. The application is built using Lazarus and Free Pascal, and it is licensed under the GNU General Public License v2.0.

## Features

- Real-time monitoring of CPU core frequencies
- Real-time monitoring of CPU core utilization
- Graphical representation of CPU data using charts
- Supports multiple CPU cores

## Requirements

- Lazarus IDE
- Free Pascal Compiler
- TAChart package (included with Lazarus)

## Installation

1. Clone the repository:
    ```sh
    git clone https://github.com/lone0/crazycpu.git
    cd crazycpu
    ```

2. Open the project in Lazarus IDE:
    - Open [crazycpu_project.lpi](http://_vscodecontentref_/0) in Lazarus IDE.

3. Add the TAChart package to your project:
    - In Lazarus IDE, go to `Package` -> `Install/Uninstall Packages...`
    - Find `TAChart` in the list and install it.

4. Build and run the project:
    - Click on `Run` -> `Build` to compile the project.
    - Click on `Run` -> `Run` to start the application.

## Usage

- The main window displays a chart with the frequency and utilization of each CPU core.
- The chart updates every second to show the latest data.

## License

This project is licensed under the GNU General Public License v2.0. See the [LICENSE](http://_vscodecontentref_/1) file for details.

## Contributing

Contributions are welcome! Please fork the repository and submit a pull request with your changes.

## Acknowledgements

- [Lazarus IDE](https://www.lazarus-ide.org/)
- [Free Pascal](https://www.freepascal.org/)
- [TAChart](https://wiki.lazarus.freepascal.org/TAChart)

## Contact

For any questions or suggestions, please open an issue on the [GitHub repository](https://github.com/lone0/crazycpu/issues).