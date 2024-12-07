# monte-carlo_monty-hall

A project focused around examples of monte-carlo integration. Additionally, the Monty-Hall problem exists.

## Setup

To build the dependencies for this repository locally, run the following command in nixOS.

```
sudo nixos-rebuild switch -I nixos-config=configuration.nix
```

If you are running on a system other than nixOS-wsl, try using the file "configuration-not-wsl.nix" instead

Then, to install python dependencies, run the following command in the calculating-pi directory:

```
poetry install
```

Then, to run the project:

```
poetry run python main.py
```
