alter text search configuration portuguese alter mapping for word, hword, hword_part with unaccent, portuguese_stem;
update paragrafodiario set portuguese_conteudo_tsvector=to_tsvector('portuguese', conteudo);