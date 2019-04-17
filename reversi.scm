;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reversi
;; Copyright (C) 2005 by 日置尋久
;; modified only a litte bit by T. Sakuragawa
;; 1+, "〇 "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;-----------------------------------------
;; 大局変数
;;-----------------------------------------

;; 盤面サイズ
(define *board-size* 8)
(define *board-points* (* *board-size* *board-size*))


;;
;; 盤面上のマスは(u v)で指定する(0<= u,v <= 7)．
;; (i j)は，上からi行め，左からj列めのマスを示す
;; (0 7)は，右上角，(7 0)は，左下角
;; 
;; 盤面データ
;;
;; *empty*  --- 空
;; *black*  --- 黒
;; *white*  --- 白
;; *wall*   --- 境界
;;
;; 盤面の境界での処理を簡単にするため，盤面は，
;; *board-size*より一回り大きく定義してある．
;; 縁のマス{(-1,*),(*,-1),(8,*),(*,8)}には，
;; 予め白でも黒でもない*wall*を置いておく．
;;
(define *empty* 0)
(define *black* 1)
(define *white* 2)
(define *wall* 3)

;;
;; 方向(du dv)のリスト
;; (右 右上 上 左上 左 左下 下 右下)
;; を意味する．
;;
(define *directions* '((0 1) (-1 1) (-1 0) (-1 -1) 
		       (0 -1) (1 -1) (1 0) (1 1)))

;;-----------------------------------------
;; 主な関数(詳細は各関数の定義を参照のこと)
;;-----------------------------------------
;;
;; game:            reversiの対戦を行う
;; man-move:        ユーザによる操作関数
;; do-move:         石を置く
;; moves:           候補手のリストを生成する
;; bw-count:        盤面上の白黒の個数を返す
;; p-count:         盤面上の白黒いずれかの個数を返す
;; opponent:        相手の色
;; board-status:    盤面の指定された場所の状態を返す
;; board-set!:      盤面の指定された場所の状態の設定
;; empty?:          盤面の指定された場所は空いているか??
;; duplicate-board: 盤面の複製を作る(新しい盤面を生成する)
;; copy-board:      盤面データのコピー
;; display-board:   盤面の表示
;; dump-board:      盤面上の石データをリストにして返す
;; board-size-set!: 盤面サイズの変更
;;
;;
;; create-given-board:   与えられた状態の盤面の生成
;; create-initial-board: 初期盤面の作成
;; init-board:           盤面の初期化
;; create-board:         盤面の作成
;; reset-board:          盤面の再初期化
;;
;;-------------------------------------

;;
;; game: reversiの対戦を行う
;;
;; 引数
;;   black-player-move 黒番の操作を行う関数
;;   white-player-move 白番の操作を行う関数
;;   これらの関数は，手を決めて石を配置する処理を適宜行う
;;   ものとする．また次のように真偽値を返すものとする．
;;     1. ゲームを続行する場合は#t
;;     2. 投了する場合は#f
;;
;; man-moveを使うとユーザが操作を行うことになる．
;;
;;
;; 
;; なお，局面と手番を引数としてさらに追加で与えると，その状態から始める
;; 何も与えない場合には初期状態から始める
;; (局面と手番を与える場合には引数は全部で四つになる)
;; 
;; ある局面の盤面を生成するには関数create-given-boardを使う
;; また関数dump-boardで盤面のデータを出力できる．その出力データは
;; create-given-boardにそのまま与えることができる．
;;
(define (game black-player-move white-player-move . current-status)
  ;;
  ;; game-aux
  ;;
  ;; 引数
  ;;   bw     手番(*black*,*white*)
  ;;   board  現在の局面データ
  ;;   count  盤面上の石の個数
  ;;   passed 相手が直前にパスしたか(#t/#f)
  ;;
  (define (game-aux bw board count passed)
    ;;局面の表示
    (newline)
    (display-board board)
    (newline)
    (if (or (= count *board-points*) (= (p-count bw board) 0))
	;; 盤面が全て埋めつくされているか，
	;; 手番のプレイヤbwの石がないなら終了
	(game-end-message board)  
	;;一手進める
	(if (null? (moves bw board)) 
	    ;; bwの打つ手がない場合(PASS)
	    (if passed
		;; 直前に相手もPASSをしていた場合
		;; お互いに打つところがない→終了
		(game-end-message board)
		;; そうでない場合は，相手の手番に移る
		(begin
		  (display (string-append (players-name bw) " PASS"))
		  (newline)
		  ;;次の手番(bwはPASSした)
		  (game-aux (next-turn bw) board count #t)
		  ))
	    ;; bwに手を決めて石を置いてもらう
	    (if 
	     (if (= bw *black*)
		 (black-player-move bw board)
		 (white-player-move bw board))
	      ;;次の手番(*-player-moveが#tを返した場合)
	     (game-aux  (next-turn bw) board (+ 1 count) #f)
	     ;;投了(*-player-moveが#fを返した場合)
	     (game-end-message board)))))
  ;; gameの本体(game-auxの呼びだし)
  (if (null? current-status)
      ;; 現在の状態が与えられない場合は，初期状態からスタート
      (game-aux *black* (create-initial-board) 4 #f)
      ;; 現在の状態が与えられた場合は，そこからスタート
      ;; (注::引数のチェックはしていない)
      (let* ((board (car current-status))
	     (count (bw-count board))
	     (bw (cadr current-status)))
	(game-aux bw board (+ (car count) (cadr count)) #f)
      )))


;;
;; man-move: MANプレイヤ操作関数
;;
;; 引数
;;   bw    手番
;;   board 局面
;;
;; 手を(y x)で入力して 局面を進めて，#tを返す．
;; 入力が^Dであった場合は，投了であるとして，#fを返す．
;;
(define (man-move bw board)
  (define (display-prompt bw)
	   (display (string-append (players-name bw) ">> ")))
  (call-with-current-continuation
   (lambda (return)
     (begin 
       ;; プロンプト
       (display-prompt bw)
       ;; 手を読み込む
       (do ((hand (read) (read)))
	   ((cond ((eof-object? hand) (return #f)) ;; ^Dなら#fを返して終了
		  ((out-of-bounds? hand) #f)       ;; 手が無効(盤の外)
		  (#t (do-move bw hand board)))    ;; 指定された石を置く
	    #t
	    )
	 (display "そこには置けません")
	 (newline)
	 (display-prompt bw)
	 )))))


;;
;; do-move: 石を置く
;;
;; 引数
;;   bw    手番
;;   point 石を置く場所 (y x)
;;   board 局面
;;
;; bwの手番で，pointに石を置く．
;; 手が有効であれば，石を置く処理を行って#tを返す．
;; そうでなければ，#fを返す．
;;
(define (do-move bw point board)
  ;;
  ;; do-move-aux: 石を置く処理を行う
  ;;
  ;; 引数 
  ;;   dirs 反転処理を行う方向のリスト
  ;; 
  ;; dirsが空であれば，石をpointに置く．
  ;; そうでなければ，dirsに含まれる方向について石の反転処理を行う
  ;;
  (define (do-move-aux dirs)
    (if (null? dirs)
	(begin 
	  (board-set! bw point board)
	  #t
	  )
	(begin
	  (do-flip-dir bw point (car dirs) board)
	  (do-move-aux (cdr dirs))
	  )))
  (let ((d (run-dirs bw point board)))
    (if (null? d)
	#f
	(do-move-aux d))))

;;
;; moves: 候補手のリストを生成する．
;;
;; 引数
;;   bw    手番
;;   board 局面
;;
(define (moves bw board)
  (do ((i 0 (+ i 1))
       (pool '()))
      ((= i *board-size*) pool)
    (do ((j 0 (+ j 1)))
	((= j *board-size*) pool)
      (let ((p (run-dirs bw (list i j) board)))
	(if (null? p)
	    p
	    (set! pool (append pool (list (list i j)))))))))


;;
;; bw-count: 盤面上の白黒の個数を返す．
;;
;; 引数
;;   board 局面
;;
;; (黒の個数 白の個数)というリストを返す
;;
(define (bw-count board)
  (do ((i 0 (+ i 1))
       (count '(0 0)))
      ((= i *board-size*) count)
    (do ((j 0 (+ j 1)))
	((= j *board-size*))
      (let ((bw (board-status (list i j) board)))
	(cond ((= bw 1) (set! count (list (+ (car count) 1) (cadr count))))
	      ((= bw 2) (set! count (list (car count)  (+ 1 (cadr count)))))
	      (#t #f))))))

;;
;; p-count: 盤面上の白黒いずれかの個数を返す．
;;
;; 引数
;;   bw    色
;;   board 局面
;;
(define (p-count bw board)
  (let ((c (bw-count board)))
    (if (= bw *black*)
	(car c)
	(cadr c))))


;;
;; do-flip-dir: 1方向に関して石の反転処理を行う．
;;
;; 引数
;;   bw    手番
;;   point 石を置く場所
;;   dir   方向
;;   board 局面
;;
;; pointのdir方向にbwへの反転を実行する．
;; dir方向の隣接点から反転することに注意．
;; (point自体については操作しない)
;;
(define (do-flip-dir bw point dir board)
  (let* ((seq (flippable-range+-dir bw point dir board))
	 (seq-end (cadr seq)))
    (do ((p (neighbor point dir) (neighbor p dir)))
	((the-same-point? p seq-end))
      (board-set! bw p board))))

;;
;; run-dirs: 石を挟める方向を列挙する
;; 
;; 引数
;;   bw    手番
;;   point 石を置く場所
;;   board 局面
;; 
;; bwの手番でpointで石を挟める方向を列挙する
;;
(define (run-dirs bw point board)
  (define (aux p dirs)
    (cond ((null? dirs) p)
	  ((null? (flippable-range+-dir bw point (car dirs) board)) 
	   (aux p (cdr dirs)))
	  (#t (aux (append p (list (car dirs))) (cdr dirs)))))
  (aux '() *directions*))
	
;;	
;; flippable-range+-dir: 1方向に関して石の挟むことのできる範囲を返す．
;;
;; 引数
;;   bw    手番
;;   point 石を置く場所
;;   dir   方向
;;   board 局面
;;
;; bwの手番でpointのdir方向で挟むことのできる石の範囲を返す．
;; 挟める石がなければ()を返す．
;; 範囲は，(反転できる始点 反転できる終点の隣)を返す
;; 例:
;; (flippable-range+-dir 1 '(1 1) '(0 1) board)
;; --> ((1 2) (1 6)) 
;; (1 2) (1 3) (1 4) (1 5)が反転できることを意味する．
;; つまり，(1 2)--(1,5)は相手の石，(1 6)はbwの石である．
;;
(define (flippable-range+-dir bw point dir board)
  (if (and (empty? point board)
	   (valid-direction? dir))
      (let* ((s-end (run-end point dir (opponent bw) board))
	     (s-end-next (neighbor s-end dir)))
	(cond ((the-same-point? s-end point) '())
	      ((eq? (board-status s-end-next board) bw)
	       (list (neighbor point dir) s-end-next))
	      (#t '())))
      '()))

;;
;; run-end: 同一色のrunの終点を調べる．
;;
;; 引数
;;   point 調べ始める場所
;;   dir   方向
;;   bw    調べる色
;;   board 局面
;;
;; pointのdir方向の隣接点から延びるbwのrunの終点を調べる．
;; そのようなrunがなければ，pointそのものを返す．
;;
(define (run-end point dir bw board)
  (let ((p (neighbor point dir)))
    (if (= (board-status p board) bw)
	(run-end p dir bw board)
	point)))

;; opponent: 相手の色
;;   bw == *black* --> *white*
;;   bw == *white* --> *black*
(define (opponent bw) (- *wall* bw))

;; 次の手番
(define next-turn opponent)

;; board-status: 盤面の指定された場所の状態を返す
(define (board-status point board)
  (let ((col (car point))
	(row (cadr point)))
    (and (array-in-bounds? board col row)
	 (array-ref board col row))))

;; empty?: 盤面の指定された場所は空いているか??
(define (empty? point board)
  (= (board-status point board) *empty*))

;; board-set!: 盤面の指定された場所の状態の設定
(define (board-set! val point board)
  (let ((col (car point))
	(row (cadr point)))
    (and (array-in-bounds? board col row)
	 (array-set! board val col row))))


;; dirは有効な方向の指定か?
(define (valid-direction? dir)
  (let ((dc (car dir))
	(dr (cadr dir)))
    (and 
     (and (<= (abs dc) 1) (<= (abs dr) 1))
     (not (and (= dc 0) (= dr 0))))))

;; xとyは同一地点か??
(define (the-same-point? x y)
  (and (= (car x) (car y))
       (= (cadr x) (cadr y))))


;; xのd方向の位置
(define (neighbor x d)
  (list (+ (car x) (car d)) (+ (cadr x) (cadr d))))

;; bwのプレイヤー名
(define (players-name bw)
  (if (= bw *black*)
      "black"
      "white"))

;; pointが盤面の境界を越えているか??
(define (out-of-bounds? point)
  (let ((y (car point))
	(x (cadr point)))
    (or (< y 0) (< x 0) (>= y *board-size*) (>= x *board-size*))))

;; 終了メッセージ
(define (game-end-message board)
  (display "GAME OVER")
  (newline)
  (display "Black vs White ")
  (display (bw-count board))
  (newline)
  (newline))

;; display-board: 盤面の表示
(define (display-board board)
  (define (display-row-numbers)
    (display " ")
    (do ((i 0 (+ i 1)))
	((= i *board-size*))
      (display " ")
      (display i))
    (newline))
  (display-row-numbers)
  (do ((i 0 (+ i 1)))
      ((= i *board-size*))
    (display i)
    (do ((j 0 (+ j 1)))
	((= j *board-size*))
      (let ((t (array-ref board i j)))
	(display 
	 (cond ((= t *empty*) " □")
	       ((= t *black*) " ●")
	       ((= t *white*) " ○")
	       (#t #f)))))
    (display i)
    (newline))
  (display-row-numbers))

;; duplicate-board: 盤面の複製(同じ内容の新しい盤面データを作る)
(define (duplicate-board board)
  (let ((newboard (create-board)))
    (copy-board newboard board)
  newboard))

;; copy-board: 盤面のコピー(src盤面の内容をdst盤面にコピーする)
(define (copy-board dst src)
  (do ((i -1 (+ i 1)))
      ((> i *board-size*) #t)
    (do ((j -1 (+ j 1)))
	((> j *board-size*))
      (array-set! dst (array-ref src i j) i j))))

;; create-given-board:
;;
;; 盤面データを次の形式で渡して，その状態の盤面を生成する
;; (黒石のマスのリスト 白石のマスのリスト)
;; 
;; (例) (((0 0) (0 1) ... ) ((2 3) (4 5) ... ))
;;
(define (create-given-board bwdata)
  (define (put-bw bw board points)
    (if (null? points)
	board
	(let ((point (car points)))
	  (if (or (out-of-bounds? point) (not (empty? point board)))
	      (put-bw bw board (cdr points))
	      (begin
		(board-set! bw point board)
		(put-bw bw board (cdr points)))))))
  (let ((board (create-board)))
    (put-bw *black* board (car bwdata))
    (put-bw *white* board (cadr bwdata))
    board))

;; 盤面board上の石データをリストにして返す
;; リストはcreate-given-boardの引数の形式になる．
(define (dump-board board)
  (do ((i 0 (+ i 1))
       (black '() black)
       (white '() white))
      ((= i *board-size*) (list black white))
    (do ((j 0 (+ j 1)))
	((= j *board-size*))
      (let* ((point (list i j))
	     (bw (board-status point board)))
	(cond ((= bw *black*) (set! black (append black (list point))))
	      ((= bw *white*) (set! white (append white (list point)))))))))

;; create-initial-board: 初期盤面の作成
(define (create-initial-board)
  (init-board (create-board)))

;; reset-board: 盤面の再初期化(初期化して#tを返す)
(define (reset-board board)
  (init-board board)
  #t)

;; init-board: 盤面の初期化
;; 境界に「壁」をダミーとして置いてある．
;; 添字の範囲は(-1,*board-size*)x(-1,*board-size*)
(define (init-board board)
  (let ((c1 (/ *board-size* 2))         
	(c2 (- (/ *board-size* 2) 1)))
    (do ((i 0 (+ i 1)))
	((= i *board-size*))
      (do ((j 0 (+ j 1)))
	  ((= j *board-size*))
	(array-set! board *empty* i j)))
    ;; 中心の4マスを初期化する
    (array-set! board *black* c1 c2)
    (array-set! board *black* c2 c1)
    (array-set! board *white* c1 c1)
    (array-set! board *white* c2 c2)
    (do ((i -1 (+ i 1)))
	((> i *board-size*))
      (array-set! board *wall* -1 i)
      (array-set! board *wall* *board-size* i)
      (array-set! board *wall* i -1)
      (array-set! board *wall* i *board-size*)
      ))
  board)

;; create-board: 盤面の作成
(define (create-board)
  (make-array *empty* (list -1 *board-size*) (list -1 *board-size*)))

;; board-size-set!: 盤面サイズの変更
(define (board-size-set! n)
  (if (or (< n 4) (odd? n))
      #f
      (begin
	(set! *board-size* n)
	(set! *board-points* (* n n)))))

