# Lean4 → Python 翻译要求

## 忠实地将 Lean4 代码翻译为 Python

### 基本要求

1. **保持逻辑完全一致**：即使原 Lean 代码有逻辑错误，也要原样翻译，不做修正
2. **添加 `if __name__ == "__main__": main()` 调用**
3. **逐词对照翻译**：Lean 中的函数和语法在 Python 中尽量找对应的实现
   - `println` → `print`
   - `match ... with` → `match ...:`（使用 Python match 语句）
   - 其他 Lean 关键字/函数找最接近的 Python 等价物
4. **只使用标准库**：不允许引入第三方包（如 numpy、pandas 等）

### 注释要求

在 Python 文件顶部添加 Lean 源代码作为注释：
- 使用多行字符串 `"""..."""` 包裹
- 包含 markdown 代码块标记 ```lean
- Lean 代码不缩进，保持原始格式

示例：
```python
"""
```lean
def read : IO String := do
  let stdin ← IO.getStdin
  ...
```
"""

def read() -> str:
    ...
```

### 文件命名

- Python 文件名与 Lean 文件名保持一致
- 扩展名改为 `.py`
- 放在同一目录下

### 同步更新

当 Lean 文件被修改时，必须同步更新 Python 文件顶部的 Lean 注释。

### 测试要求

翻译完成后，使用相同的测试用例测试 Python 和 Lean4 的输出结果：
- Lean4：`lean --run xxx.lean`
- Python：`python xxx.py`

测试完成后，用表格显示测试用例和两种语言的输出结果。
