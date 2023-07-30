import hypothesis.strategies as st
import hypothesis.extra.numpy as npst
from hypothesis import given, settings
import numpy as np
import random

fornecedores = [
    "EDP Comercial",
    "Galp Energia",
    "Iberdrola",
    "Endesa",
    "Gold Energy",
    "Coopernico",
    "Enat",
    "YIce",
    "MEO Energia",
    "Muon",
    "Luzboa",
    "Energia Simples",
    "SU Electricidade",
    "EDA"
]

marca = [
    "Sony",
    "Marshall",
    "JBL",
    "Sennheiser",
    "Bowers&Wilkins",
    "Philips"
]

estacao = [
    "Comercial", 
    "RFM", 
    "Noticias", 
    "AUMINHO", 
    "MEGAHITS", 
    "M80", 
    "RTP",
    "Renascenca"
    ]

nomes = [
    "Diana",
    "Rui",
    "Afonso",
    "Bea",
    "Gongas",
    "Artur"
]

rooms = [
    "Quarto",
    "Cozinha",
    "Sala",
    "Sotão",
    "Cave",
    "Sala de Jantar"
]

colors = [
    "Vermelho",
    "Amarelo",
    "Azul",
    "Laranja",
    "Verde",
    "Roxo",
    "Branco"
]

rez = [
    "(1024x768)",
    "(1366x768)",
    "(1920x1080)",
    "(2160x1440)",
    "(3840x2160)"
]

@st.composite
def fornecedor_strategy(draw):
    return draw(st.sampled_from(fornecedores))

@st.composite
def name_strategy(draw):
    return draw(st.sampled_from(nomes))

@st.composite
def room_strategy(draw):
    return draw(st.sampled_from(rooms))

@st.composite
def casa_strategy(draw):
    nome = draw(name_strategy())
    nif = draw(st.integers(min_value=100000000, max_value=999999999))
    fornecedor = draw(fornecedor_strategy())
    return f"Casa:{nome},{nif},{fornecedor}"

@st.composite
def divisao_strategy(draw):
    nome = draw(room_strategy())
    return f"Divisao:{nome}"

@st.composite
def fn_strategy(draw):
    nome = draw(fornecedor_strategy())
    return f"Fornecedor:{nome}"

@st.composite
def smartbulb_strategy(draw):
    color = draw(st.sampled_from(colors))
    intensity = draw(st.integers(min_value=0, max_value=20))
    power = draw(st.floats(min_value=1.0, max_value=10.0))
    power = round(power, 3)  # Limit to 3 decimal places
    return f"SmartBulb:{color},{intensity},{power}"

@st.composite
def smartcamera_strategy(draw):
    resolution = draw(st.sampled_from(rez))
    fps = draw(st.integers(min_value=20, max_value=160))
    power = draw(st.floats(min_value=0.0, max_value=10.0))
    power = round(power, 3)  # Limit to 3 decimal places
    return f"SmartCamera:{resolution},{fps},{power}"

@st.composite
def smartspeaker_strategy(draw):
    volume = draw(st.integers(min_value=0, max_value=100))
    channel = draw(st.sampled_from(estacao))
    brand = draw(st.sampled_from(["Sony", "Marshall", "JBL", "Sennheiser", "Bowers&Wilkins", "Philips"]))
    power = draw(st.floats(min_value=0.0, max_value=10.0))
    power = round(power, 3)  # Limit to 3 decimal places
    return f"SmartSpeaker:{volume},{channel},{brand},{power}"

@st.composite
def registro_strategy(draw):
    return draw(st.one_of(
        fn_strategy(),
        casa_strategy(),
        divisao_strategy(),
        smartbulb_strategy(),
        smartcamera_strategy(),
        smartspeaker_strategy(),
    ))

@settings(max_examples=300)
@given(st.lists(registro_strategy(), min_size=100, max_size=150, unique=True))

def generate_file(registros):
    file_content = "\n".join(registros)
    with open("Logs/LogsPY.txt", "w") as f:
        f.write(file_content)

generate_file()
