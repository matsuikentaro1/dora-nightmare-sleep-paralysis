"""
sleep paralysis関連キーワードのスペルミス・バリエーションを実データから探索するスクリプト
find_dream_variants.pyと同じアプローチ（レーベンシュタイン距離ベース）
"""
import pandas as pd
import re
from collections import Counter


def levenshtein_distance(s1, s2):
    """レーベンシュタイン距離を計算"""
    if len(s1) < len(s2):
        return levenshtein_distance(s2, s1)

    if len(s2) == 0:
        return len(s1)

    previous_row = range(len(s2) + 1)
    for i, c1 in enumerate(s1):
        current_row = [i + 1]
        for j, c2 in enumerate(s2):
            insertions = previous_row[j + 1] + 1
            deletions = current_row[j] + 1
            substitutions = previous_row[j] + (c1 != c2)
            current_row.append(min(insertions, deletions, substitutions))
        previous_row = current_row

    return previous_row[-1]


def find_similar_words(text, base_word, max_distance=2):
    """
    base_wordに似た単語を見つける（レーベンシュタイン距離ベース）
    """
    if pd.isna(text):
        return []

    words = re.findall(r'\b\w+\b', text.lower())
    similar = []

    for word in words:
        # 長さが大きく違う場合はスキップ
        if abs(len(word) - len(base_word)) > max_distance:
            continue

        # レーベンシュタイン距離を計算
        distance = levenshtein_distance(word, base_word)
        if distance <= max_distance and distance > 0:
            similar.append(word)

    return similar


def search_variants(df, base_word, max_distance=2, top_n=30):
    """特定の単語のバリエーションを検索"""
    print(f"\n{'='*60}")
    print(f"Searching for '{base_word}' variants (distance <= {max_distance})")
    print('='*60)

    variants = Counter()
    for text in df['commentary_norm']:
        if pd.isna(text):
            continue
        found = find_similar_words(text, base_word, max_distance)
        variants.update(found)

    # 正確なマッチを除外して表示
    print(f"\nFound {len(variants)} unique variants:")
    for word, count in variants.most_common(top_n):
        # 距離も表示
        dist = levenshtein_distance(word, base_word)
        print(f"  {word:<25} count={count:>4}  (distance={dist})")

    # 正確なマッチの件数
    exact_pattern = rf'\b{re.escape(base_word)}\b'
    exact_count = df['commentary_norm'].str.contains(exact_pattern, na=False, regex=True, case=False).sum()
    print(f"\nExact match '{base_word}': {exact_count}")

    return variants


def main():
    print("Loading data...")
    df = pd.read_csv('outputs/merged_reviews.csv')

    # 解析対象のみ
    analysis_df = df[df['excluded'] == 0].copy()
    print(f"Analysis dataset: {len(analysis_df)} reviews")

    # 検索対象キーワード
    keywords = [
        # Core SP terms
        'paralysis',
        'paralyzed',
        'paralysed',
        # Hallucination
        'hallucination',
        'hallucinations',
        # Medical terms
        'hypnagogic',
        'hypnopompic',
        # Presence/entity
        'demon',
        'intruder',
        # Move-related
        'immobilized',
    ]

    all_variants = {}
    for keyword in keywords:
        variants = search_variants(analysis_df, keyword, max_distance=2, top_n=20)
        all_variants[keyword] = variants

    # サマリー：実際に使えそうなスペルミスのリスト
    print("\n" + "="*60)
    print("SUMMARY: Potential typos to include in pattern matching")
    print("="*60)

    # 手動で確認すべき候補をフィルタ
    # countが1以上で、明らかに別の単語でないもの
    for keyword, variants in all_variants.items():
        print(f"\n{keyword}:")
        for word, count in variants.most_common():
            if count >= 1:
                dist = levenshtein_distance(word, keyword)
                # 明らかに別の意味の単語を除外（手動確認用）
                exclude_words = {
                    'analysis', 'catalyst', 'paralegal', 'paradise',
                    'demon', 'demons', 'lemon', 'demons',
                    'hallway', 'hall', 'halloween',
                }
                if word not in exclude_words:
                    print(f"    {word} (count={count}, dist={dist})")


if __name__ == '__main__':
    main()
