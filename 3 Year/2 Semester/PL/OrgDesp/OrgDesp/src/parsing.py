import re
import argparse

def parse_args():
    parser = argparse.ArgumentParser(prog='org_desp',
                                     usage='%(prog)s file {inscritos, users, equipa, escaloes, html} [--help|-h]',
                                     description='Organizador de Provas de Orientação')

    parser.add_argument('file', help='Ficheiro a processar.')
    subparsers = parser.add_subparsers(dest='subcommand', required=True)

    # Imprime o nome de todos os concorrentes que se inscreverem como 'Individuais' e são de 'Valongo'.
    parse_names = subparsers.add_parser('inscritos')
    parse_names.add_argument('-e', '--equipa',
                             action='store',
                             type=str,
                             required=True,
                             nargs='*',
                             help='Equipas a procurar')

    parse_names.add_argument('-t', '--terra',
                             action='store',
                             type=str,
                             required=True,
                             nargs='*',
                             help='Terras')

    # Imprime o nome completo, número telemóvel e prova em que está inscrito de cada concorrente cujo nome seja 'Paulo' || 'Ricardo', desde que use 'GMAIL'.
    parse_user = subparsers.add_parser('users')
    parse_user.add_argument('-n', '--names',
                            action='store',
                            type=str,
                            nargs='+',
                            required=True,
                            help='Nomes de utilizadores.')

    parse_user.add_argument('-m', '--mail',
                            action='store',
                            type=str,
                            nargs='+',
                            required=True,
                            help='Emails a procurar.')

    # Imprime toda a informação dos atletas da equipa 'Turbulentos'.
    parse_user = subparsers.add_parser('equipa')
    parse_user.add_argument('team',
                            action='store',
                            type=str,
                            nargs='*',
                            help='Nomes de utilizadores.')

    # Imprime escaloes por ordem alfabética e, para cada um indicar quantos atletas estão inscritos nesse escalão
    _parse_escaloes = subparsers.add_parser('escaloes')

    # Gera página HTML...
    _parse_html = subparsers.add_parser('html')

    return parser

def parse_data(path):
    file = open(path, "r", encoding="utf-8")
    next(file)
    next(file)
    data = file.read()
    file.close()
    data = re.split(r"{", data)
    data.pop(0)

    clean_comma = lambda x : re.split(r",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)", x)
    """

    """

    clean_space = lambda x : re.sub(r" (?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)", "", x)
    """

    """

    clean_data = lambda x : re.match(r"\n*\"[\w]*\":\"(.*)\".*", x).group(1) if x else x
    """
        Retorna o conteúdo que nos interessa da linha
        ex: "morada":"Rua 25 Abril, nº 123 - 4º Eq.",
        irá retornar 'Rua 25 Abril, nº 123 - 4º Eq.'
    
        Caso a linha em questão seja inválida, len(x)==0, retorna uma lista vazia ""
    """

    comma_clean = list(map(clean_comma, iter(data)))

    space_clean = list(map(lambda x: list(map(clean_space, iter(x)))[0:7],iter(comma_clean)))

    def warn_check(inscrito):
        if any(map(lambda x: re.search(r'^\s*$', x), iter(inscrito))):
            print(f"Inscrito inválido: {inscrito}.")
        return inscrito

    output_clean = list(map(lambda x: warn_check(list(map(clean_data, iter(x)))), iter(space_clean)))
    
    return output_clean
