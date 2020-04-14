ALTER TABLE paragrafodiario ADD COLUMN portuguese_conteudo_tsvector_generated TSVECTOR GENERATED ALWAYS AS (to_tsvector('portuguese', conteudo)) STORED;
ALTER TABLE paragrafodiario DROP COLUMN portuguese_conteudo_tsvector;
ALTER TABLE paragrafodiario RENAME COLUMN portuguese_conteudo_tsvector_generated TO portuguese_conteudo_tsvector;
CREATE INDEX paragrafodiario_portuguese_conteudo_tsvector_idx ON paragrafodiario USING GIN (portuguese_conteudo_tsvector);
DROP FUNCTION IF EXISTS update_paragrafodiario_portuguese_conteudo_tsvector() CASCADE;