module Main where

import           Data.Graph.Inductive
import           Data.GraphViz
import           Data.GraphViz.Attributes.Complete
import           Data.GraphViz.Types.Generalised   as G
import           Data.GraphViz.Types.Monadic
import           Data.Text.Lazy                    as L
import           Data.Word
import           WriteRunDot


digraph ex3 {
    graph [rankdir=LR];
    subgraph {
        node [shape=doublecircle,fixedsize=true,width=1,style=filled,color="#7f6c8a"];
        Open [label=open];
        Closed [label=closed];
    }
    subgraph {
        node [shape=circle,fixedsize=true,width=1,style=filled,color="#7f6c8a"];
        ClosedWaitingAck [label="clsd waiting\nACK"];
    }
    subgraph {
        node [shape=box,width=1,style=filled,color="#e2ceb3"];
        cancel [label=CANCEL];
        cancelAck [label=CANCEL_ACK];
    }
    Open -> cancel;
    cancel -> ClosedWaitingAck;
    ClosedWaitingAck -> cancelAck;
    cancelAck -> Closed;
}




main :: IO ()
main = do
    doDots [("ex3" , ex3)]