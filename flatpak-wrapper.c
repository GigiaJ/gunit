#include <stdlib.h>
#include <string.h>
#include <stdio.h>

extern char **environ;

void set_environment_variables() {
    for (char **env = environ; *env != 0; env++) {
        putenv(*env);
    }
}

int main(int argc, char *argv[]) {
    const char *flatpak_command = "flatpak run com.jagex.RuneScape";

    set_environment_variables();

    printf("Received arguments:\n");
    for (int i = 1; i < argc; ++i) {
        printf("%s\n", argv[i]);
    }

    size_t command_length = strlen(flatpak_command);
    for (int i = 1; i < argc; ++i) {
        command_length += strlen(argv[i]) + 1;
    }

    char *full_command = malloc(command_length + 1);
    if (!full_command) {
        perror("malloc");
        exit(EXIT_FAILURE);
    }

    strcpy(full_command, flatpak_command);
    strcat(full_command, " https://www.runescape.com/k=5/l=0/jav_config.ws"); // configURI
    strcat(full_command, " --multi-instance"); // Allows us to run two flatpaks of the same app

    printf("%s\n", full_command);

    int result = system(full_command);
    free(full_command);

    return result;
}
