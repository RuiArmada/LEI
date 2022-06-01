#!/usr/bin/env python
import argparse

from parsing import parse_args, parse_data

from tools.tools import *

def main():
    args = parse_args().parse_args()
    data = parse_data(args.file)

    if args.subcommand == 'inscritos':
        questao1(data, args.equipa, args.terra)
    elif args.subcommand == 'users':
        questao2(data, args.names, args.mail)
    elif args.subcommand == 'equipa':
        questao3(data, args.team)
    elif args.subcommand == 'escaloes':
        questao4(data)
    elif args.subcommand == 'html':
        questao5(data)

if __name__ == "__main__":
    main()
