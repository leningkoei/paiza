# Lean4 → Python 翻译要求

## 忠实地将 Lean4 代码翻译为 Python

### 基本要求

1. **保持逻辑完全一致**：即使原 Lean 代码有逻辑错误，也要原样翻译，不做修正
2. **添加 `if __name__ == "__main__": main()` 调用**

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
