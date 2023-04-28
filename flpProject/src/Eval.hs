module Eval where

import Exp
import Data.List ( union, delete, nub )

vars :: Exp -> [IndexedVar]
vars (X idxVar) = [idxVar]
vars (Lam idxVar exp) = [idxVar] ++ (vars exp)
vars (App exp1 exp2) = (vars exp1) ++ (vars exp2)

freeVars :: Exp -> [IndexedVar]
freeVars (X idxVar) = [idxVar]
freeVars (Lam idxVar exp) = delete idxVar (freeVars exp) 
freeVars (App exp1 exp2) = (freeVars exp1) ++ (freeVars exp2)

occursFree :: IndexedVar -> Exp -> Bool
occursFree idxVar exp = elem idxVar (freeVars exp)

freshVar :: IndexedVar -> [IndexedVar] -> IndexedVar
freshVar idxVar list = if elem idxVar list then freshVar (IndexedVar (ivName idxVar) (ivCount idxVar + 1)) list
                       else idxVar

renameVar :: IndexedVar -> IndexedVar -> Exp -> Exp
renameVar toReplace replacement (X idxVar) = if toReplace == idxVar then (X replacement)
                                                else (X idxVar)
renameVar toReplace replacement (Lam idxVar exp) = if toReplace == idxVar then (Lam replacement (renameVar toReplace replacement exp)) 
                                                    else (Lam idxVar (renameVar toReplace replacement exp))
renameVar toReplace replacement (App exp1 exp2) = App (renameVar toReplace replacement exp1) (renameVar toReplace replacement exp2)

substitute :: IndexedVar -> Exp -> Exp -> Exp
substitute toReplace replacement (X var) = if toReplace == var then replacement
                                           else (X var)

substitute toReplace replacement (Lam var exp) = if toReplace == var then (Lam var exp)
                                                 else (Lam var (substitute toReplace replacement exp))

substitute toReplace replacement (App exp1 exp2) = App (substitute toReplace replacement exp1) (substitute toReplace replacement exp2)

normalize :: Exp -> Exp
normalize (X var) = (X var)
normalize (Lam var exp) = if (occursFree var exp) then (Lam (freshVar var (freeVars exp)) (normalize (substitute var (X (freshVar var (freeVars exp))) exp)))
                          else (Lam var (normalize exp))
normalize (App exp1 exp2) = App (normalize exp1) (normalize exp2)

