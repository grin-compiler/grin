(module
  (type (;0;) (func))
  (type (;1;) (func (param i32 i32)))
  (type (;2;) (func (param i32)))
  (type (;3;) (func (param i32 i32 i32)))
  (import "env" "__linear_memory" (memory (;0;) 1))
  (import "env" "__indirect_function_table" (table (;0;) 0 anyfunc))
  (import "env" "__stack_pointer" (global (;0;) (mut i32)))
  (import "env" "_prim_int_print" (func (;0;) (type 2)))
  (func (;1;) (type 0)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    get_global 0
    set_local 0
    i32.const 16
    set_local 1
    get_local 0
    get_local 1
    i32.sub
    set_local 2
    get_local 2
    set_global 0
    i32.const 0
    set_local 3
    get_local 3
    i32.load align=1
    set_local 4
    i32.const 8
    set_local 5
    get_local 4
    get_local 5
    i32.add
    set_local 6
    get_local 3
    get_local 6
    i32.store align=1
    i32.const 1
    set_local 7
    get_local 4
    get_local 7
    i32.store offset=4 align=1
    get_local 4
    get_local 3
    i32.store align=1
    get_local 3
    i32.load align=1
    set_local 8
    get_local 8
    get_local 5
    i32.add
    set_local 9
    get_local 3
    get_local 9
    i32.store align=1
    i32.const 100
    set_local 10
    get_local 8
    get_local 10
    i32.store offset=4 align=1
    get_local 8
    get_local 3
    i32.store align=1
    get_local 3
    i32.load align=1
    set_local 11
    i32.const 12
    set_local 12
    get_local 11
    get_local 12
    i32.add
    set_local 13
    get_local 3
    get_local 13
    i32.store align=1
    get_local 11
    get_local 8
    i32.store offset=8 align=1
    get_local 11
    get_local 4
    i32.store offset=4 align=1
    get_local 11
    get_local 7
    i32.store align=1
    get_local 3
    i32.load align=1
    set_local 14
    get_local 14
    drop
    get_local 14
    get_local 5
    i32.add
    set_local 15
    get_local 3
    get_local 15
    i32.store align=1
    get_local 14
    get_local 11
    i32.store offset=4 align=1
    i32.const 2
    set_local 16
    get_local 14
    get_local 16
    i32.store align=1
    get_local 2
    get_local 14
    call 2
    get_local 2
    i32.load offset=12 align=1
    set_local 17
    get_local 2
    i32.load offset=8 align=1
    drop
    get_local 2
    i32.load offset=4 align=1
    drop
    get_local 2
    i32.load align=1
    drop
    get_local 17
    call 0
    i32.const 16
    set_local 18
    get_local 2
    get_local 18
    i32.add
    set_local 19
    get_local 19
    set_global 0
    return)
  (func (;2;) (type 1) (param i32 i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    get_global 0
    set_local 2
    i32.const 32
    set_local 3
    get_local 2
    get_local 3
    i32.sub
    set_local 4
    get_local 4
    set_global 0
    get_local 1
    i32.load align=1
    set_local 5
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              get_local 5
              i32.eqz
              br_if 0 (;@5;)
              i32.const 1
              set_local 6
              get_local 5
              get_local 6
              i32.eq
              set_local 7
              get_local 7
              br_if 2 (;@3;)
              i32.const 2
              set_local 8
              get_local 5
              get_local 8
              i32.eq
              set_local 9
              get_local 9
              br_if 1 (;@4;)
              br 4 (;@1;)
            end
            get_local 1
            i32.load offset=4 align=1
            set_local 10
            get_local 1
            i32.load align=1
            set_local 11
            get_local 13
            set_local 12
            get_local 15
            set_local 14
            get_local 11
            set_local 16
            get_local 10
            set_local 17
            get_local 14
            set_local 18
            get_local 12
            set_local 19
            br 2 (;@2;)
          end
          get_local 1
          i32.load offset=4 align=1
          set_local 20
          get_local 1
          i32.load align=1
          set_local 21
          get_local 23
          set_local 22
          get_local 25
          set_local 24
          get_local 21
          set_local 16
          get_local 24
          set_local 17
          get_local 20
          set_local 18
          get_local 22
          set_local 19
          br 1 (;@2;)
        end
        get_local 1
        i32.load offset=8 align=1
        set_local 26
        get_local 1
        i32.load offset=4 align=1
        set_local 27
        get_local 1
        i32.load align=1
        set_local 28
        get_local 28
        set_local 16
        get_local 27
        set_local 18
        get_local 26
        set_local 19
      end
      get_local 19
      set_local 29
      get_local 18
      set_local 30
      get_local 17
      set_local 31
      get_local 16
      set_local 32
      get_local 32
      set_local 33
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  get_local 32
                  i32.eqz
                  br_if 0 (;@7;)
                  i32.const 1
                  set_local 34
                  get_local 33
                  get_local 34
                  i32.eq
                  set_local 35
                  get_local 35
                  br_if 3 (;@4;)
                  i32.const 2
                  set_local 36
                  get_local 33
                  get_local 36
                  i32.eq
                  set_local 37
                  get_local 37
                  br_if 4 (;@3;)
                  i32.const 3
                  set_local 38
                  get_local 33
                  get_local 38
                  i32.eq
                  set_local 39
                  get_local 39
                  br_if 1 (;@6;)
                  i32.const 4
                  set_local 40
                  get_local 33
                  get_local 40
                  i32.eq
                  set_local 41
                  get_local 41
                  br_if 2 (;@5;)
                  br 6 (;@1;)
                end
                get_local 32
                set_local 42
                get_local 31
                set_local 43
                get_local 30
                set_local 44
                get_local 29
                set_local 45
                get_local 42
                set_local 46
                get_local 44
                set_local 47
                get_local 45
                set_local 48
                get_local 43
                set_local 49
                br 4 (;@2;)
              end
              get_local 32
              set_local 50
              get_local 31
              set_local 51
              get_local 30
              set_local 52
              get_local 29
              set_local 53
              get_local 50
              set_local 46
              get_local 52
              set_local 47
              get_local 53
              set_local 48
              get_local 51
              set_local 49
              br 3 (;@2;)
            end
            get_local 32
            set_local 54
            get_local 31
            set_local 55
            get_local 30
            set_local 56
            get_local 29
            set_local 57
            get_local 54
            set_local 46
            get_local 56
            set_local 47
            get_local 57
            set_local 48
            get_local 55
            set_local 49
            br 2 (;@2;)
          end
          i32.const 8
          set_local 58
          get_local 4
          get_local 58
          i32.add
          set_local 59
          get_local 59
          get_local 30
          get_local 29
          call 3
          get_local 4
          i32.load offset=16 align=1
          set_local 60
          get_local 4
          i32.load offset=12 align=1
          set_local 61
          get_local 4
          i32.load offset=8 align=1
          set_local 62
          get_local 62
          set_local 46
          get_local 61
          set_local 47
          get_local 60
          set_local 48
          br 1 (;@2;)
        end
        i32.const 24
        set_local 63
        get_local 4
        get_local 63
        i32.add
        set_local 64
        get_local 64
        get_local 30
        call 4
        get_local 4
        i32.load offset=28 align=1
        set_local 65
        get_local 4
        i32.load offset=24 align=1
        set_local 66
        get_local 68
        set_local 67
        get_local 70
        set_local 69
        get_local 66
        set_local 46
        get_local 69
        set_local 47
        get_local 67
        set_local 48
        get_local 65
        set_local 49
      end
      get_local 49
      set_local 71
      get_local 48
      set_local 72
      get_local 47
      set_local 73
      get_local 46
      set_local 74
      get_local 0
      get_local 74
      i32.store
      get_local 0
      get_local 73
      i32.store offset=4
      get_local 0
      get_local 72
      i32.store offset=8
      get_local 0
      get_local 71
      i32.store offset=12
      i32.const 32
      set_local 75
      get_local 4
      get_local 75
      i32.add
      set_local 76
      get_local 76
      set_global 0
      return
    end
    i32.const 666
    set_local 77
    get_local 77
    call 0
    unreachable)
  (func (;3;) (type 3) (param i32 i32 i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    get_global 0
    set_local 3
    i32.const 32
    set_local 4
    get_local 3
    get_local 4
    i32.sub
    set_local 5
    get_local 5
    set_global 0
    i32.const 16
    set_local 6
    get_local 5
    get_local 6
    i32.add
    set_local 7
    get_local 7
    get_local 1
    call 2
    get_local 5
    i32.load offset=28 align=1
    set_local 8
    get_local 5
    get_local 2
    call 2
    get_local 5
    i32.load offset=12 align=1
    set_local 9
    get_local 8
    get_local 9
    i32.gt_s
    set_local 10
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            get_local 10
            br_if 0 (;@4;)
            i32.const 1
            set_local 11
            get_local 10
            get_local 11
            i32.and
            set_local 12
            get_local 12
            i32.eqz
            br_if 1 (;@3;)
            br 3 (;@1;)
          end
          get_local 14
          set_local 13
          get_local 16
          set_local 15
          i32.const 3
          set_local 17
          get_local 17
          set_local 18
          get_local 15
          set_local 19
          get_local 13
          set_local 20
          br 1 (;@2;)
        end
        i32.const 1
        set_local 21
        get_local 8
        get_local 21
        i32.add
        set_local 22
        i32.const 0
        set_local 23
        get_local 23
        i32.load align=1
        set_local 24
        i32.const 8
        set_local 25
        get_local 24
        get_local 25
        i32.add
        set_local 26
        get_local 23
        get_local 26
        i32.store align=1
        get_local 24
        get_local 22
        i32.store offset=4 align=1
        get_local 24
        get_local 23
        i32.store align=1
        get_local 23
        i32.load align=1
        set_local 27
        i32.const 12
        set_local 28
        get_local 27
        get_local 28
        i32.add
        set_local 29
        get_local 23
        get_local 29
        i32.store align=1
        get_local 27
        get_local 2
        i32.store offset=8 align=1
        get_local 27
        get_local 24
        i32.store offset=4 align=1
        get_local 27
        get_local 21
        i32.store align=1
        get_local 1
        set_local 30
        i32.const 4
        set_local 31
        get_local 31
        set_local 18
        get_local 30
        set_local 19
        get_local 27
        set_local 20
      end
      get_local 20
      set_local 32
      get_local 19
      set_local 33
      get_local 18
      set_local 34
      get_local 0
      get_local 34
      i32.store
      get_local 0
      get_local 33
      i32.store offset=4
      get_local 0
      get_local 32
      i32.store offset=8
      i32.const 32
      set_local 35
      get_local 5
      get_local 35
      i32.add
      set_local 36
      get_local 36
      set_global 0
      return
    end
    i32.const 666
    set_local 37
    get_local 37
    call 0
    unreachable)
  (func (;4;) (type 1) (param i32 i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    get_global 0
    set_local 2
    i32.const 48
    set_local 3
    get_local 2
    get_local 3
    i32.sub
    set_local 4
    get_local 4
    set_global 0
    i32.const 32
    set_local 5
    get_local 4
    get_local 5
    i32.add
    set_local 6
    get_local 6
    get_local 1
    call 2
    get_local 4
    i32.load offset=44 align=1
    drop
    get_local 4
    i32.load offset=40 align=1
    set_local 7
    get_local 4
    i32.load offset=36 align=1
    set_local 8
    get_local 4
    i32.load offset=32 align=1
    set_local 9
    i32.const 3
    set_local 10
    get_local 9
    get_local 10
    i32.eq
    set_local 11
    get_local 9
    set_local 12
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            get_local 11
            br_if 0 (;@4;)
            i32.const 4
            set_local 13
            get_local 12
            get_local 13
            i32.eq
            set_local 14
            get_local 14
            br_if 1 (;@3;)
            br 3 (;@1;)
          end
          i32.const 0
          set_local 15
          get_local 15
          set_local 16
          get_local 16
          set_local 17
          get_local 15
          set_local 18
          br 1 (;@2;)
        end
        i32.const 16
        set_local 19
        get_local 4
        get_local 19
        i32.add
        set_local 20
        get_local 20
        get_local 8
        call 2
        get_local 4
        i32.load offset=28 align=1
        set_local 21
        i32.const 8
        set_local 22
        get_local 4
        get_local 22
        i32.add
        set_local 23
        get_local 23
        get_local 7
        call 4
        get_local 4
        i32.load offset=12 align=1
        set_local 24
        get_local 21
        get_local 24
        i32.add
        set_local 25
        i32.const 0
        set_local 26
        get_local 26
        set_local 17
        get_local 25
        set_local 18
      end
      get_local 18
      set_local 27
      get_local 17
      set_local 28
      get_local 0
      get_local 28
      i32.store
      get_local 0
      get_local 27
      i32.store offset=4
      i32.const 48
      set_local 29
      get_local 4
      get_local 29
      i32.add
      set_local 30
      get_local 30
      set_global 0
      return
    end
    i32.const 666
    set_local 31
    get_local 31
    call 0
    unreachable)
  (data (i32.const 0) "\00\00\00\00"))
