# soil

An easier way to create curl commands.

## Installation

All assets for the program are stored in `$XDG_CONFIG_HOME/soil/`.  The fallback directory is `~/.config/soil/` for cases when that environment variable is not defined.

Create an `apis.yaml` file in the base directory with the following structure:
```
app-name:
  env1: https://env1.your-domain.com
  env2: https://env2.your-domain.com
```

You can add multiple apis and multiple enviroments per api. There is nothing special about the naming.
