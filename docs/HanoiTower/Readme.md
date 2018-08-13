# Functions

HanoiTower 模块

::: danger On Hold
Need reconstruction
:::

- 模块可单独加载

```haskell
Import["https://deus.netlify.com/pkg/HanoiTower.wl"];
```

## HanoiSteps
### 参数说明

::: tip HanoiSteps[n,p]
- n 类型为 `Integer`, 汉诺塔的层数
- p 类型为 `Integer`, 汉诺塔的柱数
:::

### 返回值

给出汉诺塔的最少移动步骤, n>4 的情况尚未证明.

n = 3,4,5 时使用公式, n>=6 使用 FrameStewart 算法求解.

- $H_3(n) = 2^n - 1$
- $H_4(n) = \dfrac{2^t}{4}  \left(2 n-t^2+3 t-4\right)+1$
	- 其中 $ t = \left[\sqrt{2 n}\right] $
- $H_5(n) = \dfrac{2^t}{12} \left(6 n-t^3+3 t^2-8 t+12\right)-1$
	- 其中 $ t = \left\lfloor \frac{\left(\sqrt{729 n^2-3}+27 n\right)^{2/3}+\sqrt[3]{3}}{3^{2/3} \sqrt[3]{\sqrt{729 n^2-3}+27 n}}\right\rfloor $

一些研究表明更高阶也有此类形式.

### 标准示例

#### 测试代码
```haskell
Array[HanoiSteps,10]            (*A000225*)
Table[HanoiSteps[i,4],{i,10}]   (*A007664*)
Table[HanoiSteps[i,5],{i,10}]   (*A007665*)
Table[HanoiSteps[i,6],{i,10}]   (*A182058*)
```

#### 测试输出:
```haskell
{ 1, 3, 7, 15, 31, 63, 127, 255, 511, 1023 }
{ 1, 3, 5, 9,  13, 17, 25,  33,  41,  49   }
{ 1, 3, 5, 7,  11, 15, 19,  23,  27,  31   }
{ 1, 3, 5, 7,  9,  13, 17,  21,  25,  29   }
```

## HanoiMove
### 参数说明

::: tip HanoiMove[n,p]
- n 类型为 `Integer`, 汉诺塔初态的层数
- p 类型为 `Integer`, 汉诺塔初态的柱数
:::

::: tip HanoiMove[start,finish]
- start  类型为 `List`, 汉诺塔的起始状态
- finish 类型为 `List`, 汉诺塔指定的终态
- 仅限三阶汉诺塔
:::


### 返回值

给出汉诺塔状态间的最优移动方式

### 标准示例

#### 测试代码:
```haskell
Import[]
```

#### 测试输出:


## HanoiGraph
### 参数说明

::: tip HanoiGraph[n]
- n 类型为 `Integer`, 汉诺塔的阶数
:::

### 可选项



### 返回值

给出n阶汉诺图

### 标准示例

#### 测试代码:
```haskell
Import[]
```

#### 测试输出:






## HanoiShow
### 参数说明

::: tip HanoiShow[states_]
- states 类型为
:::

### 可选项



### 返回值

可视化圆盘的移动过程

### 标准示例

#### 测试代码:
```haskell
Import[]
```

#### 测试输出:


