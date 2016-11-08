{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Database.Oracle.Enumerator

main :: IO ()
main = loop 0 $ connect "cpf" "cpf" "oracle/ORA11G"
    where loop i dbh = do
              (_, newdbh) <- withContinuedSession dbh $ do
                  execDDL qCreate
                  _ <- execDML qInsert
                  execDDL qDrop
                  commit
              print i
              loop (i+1) newdbh

qCreate = sql "create global temporary table callRows (id NUMBER) on commit delete rows"
qInsert = cmdbind "insert into callrows (id) select ? from dual" [bindP (1::Int)]
--qInsert = sql "insert into callrows (id) select 1 from dual" -- using this version there is no space leak
qDrop = sql "drop table callrows"
