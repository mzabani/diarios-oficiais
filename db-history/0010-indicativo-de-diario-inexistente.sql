alter table conteudodiario add column diario_existe bool;

-- O md5sum da string vazia logo abaixo
update conteudodiario set diario_existe = (md5sum <> 'd41d8cd98f00b204e9800998ecf8427e'); 

alter table conteudodiario alter column diario_existe set not null;