import Window
import Keyboard

type Player = { x:Float  , y:Float
              , vx:Float , vy:Float
              , rot:Float
              , projDelta:Float }

type Projectile = { x:Float  , y:Float
                  , vx:Float , vy:Float }

type GameState = { player1:Player , player2:Player
                 , score1:Int     , score2:Int
                 , projectiles:[Projectile] }

delta = inSeconds <~ fps 45

shotDelay = 0.5
gravStr = 0.004
playerAccel= 1
rotSpeed = 5
starSize = 0.05
startDistance = 0.7

type UserInput = { x1:Int , y1:Int
                 , x2:Int , y2:Int
                 , delta:Float }

input : Signal UserInput
input = sampleOn delta (UserInput <~ lift .x Keyboard.wasd 
                                   ~ lift .y Keyboard.wasd
                                   ~ lift .x Keyboard.arrows
                                   ~ lift .y Keyboard.arrows
                                   ~ delta) 

player1Start = { x = -startDistance , y = 0 , vx = 0 , vy = 0 , rot = 0 , projDelta = 0 }
player2Start = { x =  startDistance , y = 0 , vx = 0 , vy = 0 , rot = 0 , projDelta = 0 }
defaultGame = { player1 = player1Start
              , player2 = player2Start
              , score1 = 0
              , score2 = 0
              , projectiles =[] }

stepGame : UserInput -> GameState -> GameState
stepGame inp gs = gs |> stepPlayers inp
                     |> fireProjectiles inp
                     |> stepProjectiles inp.delta
                     |> checkBounds

stepProjectiles : Float -> GameState -> GameState
stepProjectiles delta gs = let gravAcc p = (-gravStr)/(p.x^2 + p.y^2)
                               stepProjectile p = { p | vx <- p.vx + (gravAcc p)*(p.x/(sqrt(p.x^2+p.y^2)))
                                                      , vy <- p.vy + (gravAcc p)*(p.y/(sqrt(p.x^2+p.y^2)))
                                                      , x <-  p.x + p.vx*delta
                                                      , y <-  p.y + p.vy*delta
                                                  }
                               removeOBProjs = filter (\p -> dist p < 1 && dist p > starSize)
                               dist p  = sqrt(p.x^2 + p.y^2)
                           in { gs | projectiles <- map stepProjectile gs.projectiles
                                                    |> removeOBProjs }

fireProjectiles : UserInput -> GameState -> GameState
fireProjectiles inp gameState= let fireProj p = { x = p.x + 0.03 * sin p.rot
                                                , y = p.y - 0.03 * cos p.rot
                                                , vx = p.vx + sin p.rot
                                                , vy = p.vy - cos p.rot}
                                   resetProjDelta p = {p | projDelta <- 0}
                                   fireP1 gs = if gs.player1.projDelta > shotDelay
                                               then {gs | player1 <- resetProjDelta gs.player1
                                                        , projectiles <- fireProj gs.player1 :: gs.projectiles}
                                               else gs
                                   fireP2 gs = if gs.player2.projDelta > shotDelay
                                               then {gs | player2 <- resetProjDelta gs.player2
                                                        , projectiles <- fireProj gs.player2 :: gs.projectiles}
                                               else gs
                                in (fireP2 . fireP1) gameState

checkBounds : GameState -> GameState
checkBounds gState = let dist p      = sqrt(p.x^2 + p.y^2)
                         sep a b = sqrt((a.x-b.x)^2 + (a.y-b.y)^2)
                         collided player projs =    dist player > 1
                                                 || dist player < starSize
                                                 || any (\x -> sep player x < 0.02) projs
                         boundsP1 gs = if collided gs.player1 gs.projectiles
                                       then {gs | player1 <- player1Start
                                                , score2 <- gs.score2 + 1}
                                       else gs
                         boundsP2 gs = if collided gs.player2 gs.projectiles
                                       then { gs | player2 <- player2Start
                                                 , score1 <- gs.score1 + 1 }
                                       else gs
                     in (boundsP2 . boundsP1) gState

stepPlayers : UserInput -> GameState -> GameState
stepPlayers inp gs = let p1Input = (toFloat inp.x1, toFloat inp.y1, inp.delta)
                         p2Input = (toFloat inp.x2, toFloat inp.y2, inp.delta)
                         stepPlayer delta inp player = player |> stepPlayerInput inp
                                                              |> stepPlayerGrav
                                                              |> stepPlayerPos delta
                     in { gs | player1 <- stepPlayer inp.delta p1Input gs.player1
                             , player2 <- stepPlayer inp.delta p2Input gs.player2 }

stepPlayerInput : (Float,Float,Float) -> Player -> Player
stepPlayerInput (inpX,inpY,delta) p = {p | rot <- p.rot - rotSpeed *inpX*delta
                                         , vx <- p.vx + playerAccel*inpY*(sin p.rot)*delta
                                         , vy <- p.vy - playerAccel*inpY*(cos p.rot)*delta
                                         , projDelta <- p.projDelta + delta
                                      }

stepPlayerGrav : Player -> Player
stepPlayerGrav p = let gravAcc = (-gravStr)/(p.x^2 + p.y^2)
                   in { p | vx <- p.vx + gravAcc*(p.x/(sqrt(p.x^2+p.y^2)))
                          , vy <- p.vy + gravAcc*(p.y/(sqrt(p.x^2+p.y^2)))
                      }

stepPlayerPos : Float -> Player -> Player
stepPlayerPos delta p = {p | y  <- p.y + p.vy * delta
                           , x  <- p.x + p.vx * delta
                        }


display : (Int,Int) -> GameState -> Element
display (w,h) gs =
  let wf = toFloat w
      hf = toFloat h
      projs =  map (drawProjectile gameRad) gs.projectiles
      gameRad = min wf hf / 2
      gameBoundry = circle (gameRad)
                    |> filled black
      star = circle (gameRad*starSize)
             |> filled red
      drawPlayerScaled = drawPlayer <| gameRad
      p1Image = drawPlayerScaled yellow gs.player1
      p2Image = drawPlayerScaled green gs.player2
      s1Image = gs.score1 |> toText . show
                          |> Text.color yellow
                          |> Text.height 60
                          |> toForm . text
                          |> move (-0.9*gameRad,0.9*gameRad)
      s2Image = gs.score2 |> toText . show
                          |> Text.color green
                          |> Text.height 60
                          |> toForm . text
                          |> move (0.9*gameRad,0.9*gameRad)
  in collage w h ([gameBoundry, star, p1Image, p2Image, s1Image, s2Image ] ++ projs)

drawProjectile : Float -> Projectile -> Form
drawProjectile winScale p = let xT = winScale*p.x
                                yT = winScale*p.y
                            in circle (winScale/100)
                               |> filled white
                               |> move (xT,yT)



drawPlayer : Float -> Color ->  Player -> Form
drawPlayer winScale c p = let xT = winScale*p.x
                              yT = winScale*p.y
                          in polygon [(0,-winScale/20),(winScale/80,0),(-winScale/80,0)]
                             |> filled c
                             |> rotate p.rot
                             |> move (xT,yT)

gameState = foldp stepGame defaultGame input

main = lift2 display Window.dimensions gameState
