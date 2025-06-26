# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

"""Command-line interface for codegen."""
import click
from . import CodeGenerator


@click.command()
@click.argument("config_file", type=click.Path(exists=True))
@click.argument("output_path", type=click.Path())
@click.argument("templates0_path", type=click.Path(exists=True, file_okay=False, dir_okay=True))
@click.argument("templates1_path", type=click.Path(exists=True, file_okay=False, dir_okay=True))
def main(config_file: str, output_path: str, templates0_path: str, templates1_path: str):
    """Generate Erlang code from YAML configurations using Jinja2."""

    code_generator: CodeGenerator = CodeGenerator(config_file, output_path, templates0_path, templates1_path)

    code_generator.render()


if __name__ == "__main__":
    main()
