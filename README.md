# monte-carlo_monty-hall

A project focused around examples of monte-carlo integration. Additionally, the Monty-Hall problem exists.

## Setup

To build the dependencies for this repository locally, run the following command in nixOS. You may need to remove the WSL modules.

```
sudo nixos-rebuild switch -I nixos-config=configuration.nix
```

Then, to install python dependencies, run the following command in the calculating-pi directory:

```
poetry install
```

Then, to run the project:

```
poetry run python main.py
```
