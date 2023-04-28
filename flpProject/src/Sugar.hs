module Sugar where
import Exp

desugarVar :: Var -> IndexedVar
desugarVar (Var s) = IndexedVar s 0

sugarVar :: IndexedVar -> Var
sugarVar (IndexedVar s x) = if x == 0 then Var s
                            else Var (s ++ "_" ++ show x)

desugarExp :: ComplexExp -> Exp
desugarExp (CX var) = X (desugarVar var)
desugarExp (CLam var exp) = Lam (desugarVar var) (desugarExp exp)
desugarExp (CApp exp1 exp2) = App (desugarExp exp1) (desugarExp exp2)

sugarExp :: Exp -> ComplexExp
sugarExp (X idxVar) = CX (sugarVar idxVar)
sugarExp (Lam idxVar exp) = CLam (sugarVar idxVar) (sugarExp exp)
sugarExp (App exp1 exp2) = CApp (sugarExp exp1) (sugarExp exp2)
