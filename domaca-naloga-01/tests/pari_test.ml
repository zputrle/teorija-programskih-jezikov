module U = Utils;;

U.ensure_equal "{10, 20}" "{10, 20}";;
U.ensure_equal_only_with_eager_eval "{10, 20 + 30}" "{10, 50}";;
U.ensure_equal_only_with_lazy_eval "{10, 20 + 30}" "{10, 20 + 30}";;
U.ensure_equal "FST {10, 20}" "10";;
U.ensure_equal "SND {10, 20}" "20";;

U.ensure_equal "FST (SND {10, {20, 30}})" "20";;

U.ensure_equal "SND {1, 2 + 3}" "5";;

U.ensure_equal "SND (SND {1, {2, {3, {4, 5}}}})" "{3, {4, 5}}";;